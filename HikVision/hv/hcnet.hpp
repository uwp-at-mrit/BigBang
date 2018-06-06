#pragma once

#include "hv/hikvision.hpp"

namespace WarGrey::SCADA {
	typedef int(*hv_chars_convert_f)(
		char* in, unsigned short isize, unsigned short iencode,
		char* out, unsigned short osize, unsigned short oencode);

	private enum class HikVisionCfg {
		_2048 = 2048,
		_5120 = 5120,
		_10240 = 10240,
		_15360 = 15360,
		_20480 = 20480
	};

	private class HikVisionNet : public WarGrey::SCADA::HikVision {
	public:
		virtual ~HikVisionNet() noexcept;

		HikVisionNet(WarGrey::SCADA::Syslog* syslog = nullptr,
			WarGrey::SCADA::HikVisionCfg max_alarm = HikVisionCfg::_2048,
			WarGrey::SCADA::HikVisionCfg max_user = HikVisionCfg::_2048);

	public:
		bool tcp_port_cfg(unsigned short* min_port = nullptr, unsigned short* max_port = nullptr);
		bool tcp_port_cfg(unsigned short min_port, unsigned short max_port);
		bool udp_port_cfg(unsigned short* min_port = nullptr, unsigned short* max_port = nullptr);
		bool udp_port_cfg(unsigned short min_port, unsigned short max_port);
		bool memory_pool_cfg(unsigned long* max_block = nullptr, unsigned long* alarm_interval = nullptr, unsigned long* object_interval = nullptr);
		bool memory_pool_cfg(unsigned long max_block, unsigned long alarm_interval, unsigned long object_interval);
		bool module_timeout_cfg(unsigned long* prev = nullptr, unsigned long* alarm = nullptr, unsigned long* vod = nullptr, unsigned long* otherwise = nullptr);
		bool module_timeout_cfg(unsigned long prev, unsigned long alarm, unsigned long vod, unsigned long otherwise);
		bool ability_parse_cfg(bool* enabled = nullptr);
		bool ability_parse_cfg(bool enabled);
		bool talk_mode_cfg(bool* enabled = nullptr);
		bool talk_mode_cfg(bool enabled);
		bool check_device_cfg(unsigned long* timeout = nullptr, unsigned long* max_failures = nullptr);
		bool check_device_cfg(unsigned long timeout, unsigned long max_failures);
		bool general_cfg(bool* exn_cb_directly = nullptr, bool* not_split_record_file = nullptr, bool* resumable = nullptr, uint64* filesize = nullptr, unsigned long* resume_timeout = nullptr);
		bool general_cfg(bool exn_cb_directly, bool not_split_record_file, bool resumable, uint64 filesize, unsigned long resume_timeout);
		bool char_encode_cfg(WarGrey::SCADA::hv_chars_convert_f converter);
		bool log_cfg(unsigned short log_number);
	};
}
