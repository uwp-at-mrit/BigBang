#pragma once

#include <fstream>

#include "universe.hxx"
#include "planet.hpp"

#include "dirotation.hpp"
#include "hamburger.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ITimeline abstract : public WarGrey::SCADA::Planet {
	public:
		ITimeline(Platform::String^ caption, unsigned int initial_mode = 0) : Planet(caption, initial_mode) {}

	public:
		virtual void on_snapshot(long long timepoint_s,
			size_t addr0, size_t addrn, const char* data, size_t size,
			WarGrey::SCADA::Syslog* logger) = 0;
	};

	private class ITimeMachine abstract : public WarGrey::SCADA::IHamburger, public WarGrey::SCADA::IRotativeDirectory {
	public:
		ITimeMachine(Platform::String^ dirname, int frame_rate, WarGrey::SCADA::Syslog* logger,
			Platform::String^ file_prefix, Platform::String^ file_suffix,
			WarGrey::SCADA::RotationPeriod period, unsigned int period_count);

	public:
		virtual void construct() = 0;
		virtual void save_snapshot(long long timepoint_ms, size_t addr0, size_t addrn, const char* data, size_t size) = 0;
		virtual const char* seek_snapshot(long long* timepoint_ms, size_t* addr0, size_t* addrn) = 0;

	public:
		void push_timeline(WarGrey::SCADA::ITimeline* timeline);
		void travel(long long start_timepoint, long long stop_timepoint);
		WarGrey::SCADA::Syslog* get_logger();

	protected:
		Windows::UI::Xaml::Controls::Flyout^ user_interface() override;

	private:
		Windows::UI::Xaml::Controls::Flyout^ machine;
		WarGrey::SCADA::UniverseDisplay^ universe;
		bool ready;
	};

	private class TimeMachine : public WarGrey::SCADA::ITimeMachine {
	public:
		TimeMachine(Platform::String^ dirname, int frame_rate, WarGrey::SCADA::Syslog* logger = nullptr,
			Platform::String^ file_prefix = nullptr, Platform::String^ file_suffix = ".plc",
			WarGrey::SCADA::RotationPeriod period = WarGrey::SCADA::RotationPeriod::Hourly,
			unsigned int period_count = 1);

	public:
		void construct() override {}
		void save_snapshot(long long timepoint_ms, size_t addr0, size_t addrn, const char* data, size_t size) override;
		const char* seek_snapshot(long long* timepoint_ms, size_t* addr0, size_t* addrn) override;

	protected:
		void on_file_rotated(Windows::Storage::StorageFile^ prev_file, Windows::Storage::StorageFile^ current_file, long long timepoint) override;

	private:
		std::ofstream tmstream;
	};
}
