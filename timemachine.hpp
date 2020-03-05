#pragma once

#include <fstream>
#include <deque>

#include "universe.hxx"

#include "dirotation.hpp"
#include "hamburger.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ITimeMachineListener abstract {
	public:
		virtual void on_startover(long long departure_ms, long long destination_ms) {}
		virtual void on_timestream(long long timepoint_ms,
			size_t addr0, size_t addrn, uint8* data, size_t size,
			WarGrey::SCADA::Syslog* logger) = 0;
	};

	private class ITimeMachine abstract : public WarGrey::SCADA::IHamburger, public WarGrey::SCADA::IRotativeDirectory {
	public:
		ITimeMachine(Platform::String^ dirname, long long time_speed_mspf, int frame_rate, WarGrey::SCADA::Syslog* logger,
			Platform::String^ file_prefix, Platform::String^ file_suffix,
			WarGrey::SCADA::RotationPeriod period, unsigned int period_count);

	public:
		virtual void save_snapshot(long long timepoint_ms, size_t addr0, size_t addrn, uint8* data, size_t size,
			uint8 attachment_type = 0U, uint8* attachment = nullptr, size_t attachment_size = 0U) = 0;

		virtual uint8* seek_snapshot(long long* timepoint_ms, size_t* size, size_t* addr0,
			uint8* attachment_type = nullptr, size_t* attachment_size = nullptr) = 0;

		virtual void step();
	
	public:
		void pickup(WarGrey::SCADA::ITimeMachineListener* passenger);
		void startover(long long departure_ms, long long destination_ms);
		void travel();
		void service();
		void terminate();
		void shift_speed();
		void timeskip(long long timepoint_ms);

	public:
		WarGrey::SCADA::Syslog* get_logger();
		long long get_time_speed();
		unsigned int get_speed_shift();

	protected:
		virtual uint8* single_step(long long* timepoint_ms, size_t* size, size_t* addr0, uint8* attachment_type = nullptr, size_t* attachment_size = nullptr);

	protected:
		Windows::UI::Xaml::Controls::Flyout^ user_interface() override;

	private:
		void on_timestream(long long timepoint_ms, size_t addr0, size_t addrn, uint8* data, size_t size, bool keystream);

	private:
		Windows::UI::Xaml::Controls::Flyout^ machine;
		WarGrey::SCADA::UniverseDisplay^ universe;
		long long ms_per_frame;

	private: // never delete these listeners manually
		std::deque<WarGrey::SCADA::ITimeMachineListener*> passengers;
		long long departure;
		long long timepoint;
		long long destination;
	};

	private class TimeMachine : public WarGrey::SCADA::ITimeMachine {
	public:
		virtual ~TimeMachine() noexcept;

		TimeMachine(Platform::String^ dirname, long long time_speed, int frame_rate, WarGrey::SCADA::Syslog* logger = nullptr,
			Platform::String^ file_prefix = nullptr, Platform::String^ file_suffix = ".plc",
			WarGrey::SCADA::RotationPeriod period = WarGrey::SCADA::RotationPeriod::Hourly,
			unsigned int period_count = 1);

	public:
		void save_snapshot(long long timepoint_ms, size_t addr0, size_t addrn, uint8* data, size_t size,
			uint8 attachment_type = 0U, uint8* attachment = nullptr, size_t attachment_size = 0U) override;

		uint8* seek_snapshot(long long* timepoint_ms, size_t* size, size_t* addr0,
			uint8* attachment_type = nullptr, size_t* attachment_size = nullptr) override;

	public:
		void on_hiden() override;

	protected:
		void on_file_rotated(Windows::Storage::StorageFile^ prev_file, Windows::Storage::StorageFile^ current_file, long long timepoint) override;

	private:
		std::ofstream tmstream;
		long long ifsrc;
		long long ifutc;
		uint8* ifpool;
		size_t ifsize;
		size_t ifpos;
		size_t ifeof;
	};
}
