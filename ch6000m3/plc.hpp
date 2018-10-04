#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	bool DBX(const uint8* src, size_t idx);
	bool DBX(const uint8* src, size_t idx, size_t bidx);
	float DBD(const uint8* src, size_t idx);
	float RealData(const uint8* src, size_t idx);

	private class PLCConfirmation : public WarGrey::SCADA::MRConfirmation {
	public:
		void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override;

	public:
		virtual void pre_read_data(WarGrey::SCADA::Syslog* logger) {}
		virtual void post_read_data(WarGrey::SCADA::Syslog* logger) {}

	public:
		virtual void on_realtime_data(const uint8* db2, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_forat_data(const uint8* dqs, size_t dqc, const uint8* db20, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_analog_input_data(const uint8* db203, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_raw_analog_input_data(const uint8* db3, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_analog_output_data(const uint8* db204, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_raw_analog_output_data(const uint8* db5, size_t count, WarGrey::SCADA::Syslog* logger) {}

	public:
		virtual void on_digital_input(const uint8* db205, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_raw_digital_input(const uint8* db4, size_t count, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_raw_digital_output(const uint8* db6, size_t count, WarGrey::SCADA::Syslog* logger) {}
	};

	private class PLCMaster : public WarGrey::SCADA::MRMaster, WarGrey::SCADA::PLCConfirmation {
	public:
		PLCMaster(Syslog* alarm);

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime);

	public:
		void on_realtime_data(const uint8* db2, size_t count, Syslog* logger) override;

	private:
		float tidemark;
	};
}
