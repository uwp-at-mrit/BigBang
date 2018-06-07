#pragma once

#include <list>

#include "object.hpp"
#include "hv/hikvision.hpp"

namespace WarGrey::SCADA {
	typedef int(*hv_chars_convert_f)(char* in, unsigned short isize, unsigned short iencode, char* out, unsigned short osize, unsigned short oencode);

	private enum class HikVisionCfg {
		_2048 = 2048,
		_5120 = 5120,
		_10240 = 10240,
		_15360 = 15360,
		_20480 = 20480
	};

	private enum class HikVisionMsg { Exception, Reconnection, Success };
	private enum class HikVisionAlarmMsg { Exception, Reconnection, Success, Lost, Overflow };

	private struct HikVisionMatrics {
		unsigned long login;
		unsigned long real_play;
		unsigned long playback;
		unsigned long alarm_chan;
		unsigned long format;
		unsigned long file_search;
		unsigned long log_search;
		unsigned long serial;
		unsigned long upgrade;
		unsigned long voice_com;
		unsigned long broadcast;
	};

	private class IHikVisionExceptionHandler abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual void on_alarm_message(long user, long handle, WarGrey::SCADA::HikVisionAlarmMsg type) = 0;
		virtual void on_channel_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) = 0;
		virtual void on_preview_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) = 0;
		virtual void on_picture_preview_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) = 0;
	
	public:
		virtual void on_user_exchange_exception(long user, long handle) = 0;
		virtual void on_user_exchange_resume(long user, long handle) = 0;
		virtual void on_audio_exchange_exception(long user, long handle) = 0;
		virtual void on_playback_exception(long user, long handle) = 0;
		virtual void on_disk_format_exception(long user, long handle) = 0;
		virtual void on_passive_decoding_exception(long user, long handle) = 0;
		virtual void on_email_test_exception(long user, long handle) = 0;
		virtual void on_backup_exception(long user, long handle) = 0;
		virtual void on_network_flow_test_exception(long user, long handle) = 0;
		virtual void on_push_mode_login_success(long user, long handle) = 0;

	public:
		virtual void on_unknown_message(unsigned short type, long user, long handle) = 0;

	protected:
		virtual ~IHikVisionExceptionHandler() noexcept {}
	};

	private class HikVisionNet : public WarGrey::SCADA::HikVision {
	public:
		static void cleanup();
		static void configure(WarGrey::SCADA::Syslog* logger);
		static void configure(WarGrey::SCADA::HikVisionCfg max_alarm, WarGrey::SCADA::HikVisionCfg max_user);
		static WarGrey::SCADA::HikVisionNet* instace();

	public:
		unsigned long version(bool needs_build_number = false);
		WarGrey::SCADA::HikVisionMatrics statistics();
		WarGrey::SCADA::HikVisionMatrics abilities();

	public:
		std::list<std::string> get_local_ips(bool* enable_bind = nullptr);
		bool set_valid_ip(unsigned long index, bool bind = true);
		bool set_connection_time(unsigned long wait_time, unsigned long try_times = 0);
		bool set_receive_timeout(unsigned long timeout = 5000);
		bool set_reconnect_time(unsigned long interval, bool enabled = true);
		bool set_exception_handler(WarGrey::SCADA::IHikVisionExceptionHandler* exn_handler);

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

	private:
		virtual ~HikVisionNet() noexcept;
		HikVisionNet();
	};

	private class HikVisionExceptionHandler : public WarGrey::SCADA::IHikVisionExceptionHandler {
	public:
		void on_alarm_message(long user, long handle, WarGrey::SCADA::HikVisionAlarmMsg type) override;
		void on_channel_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) override;
		void on_preview_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) override;
		void on_picture_preview_message(long user, long handle, WarGrey::SCADA::HikVisionMsg type) override;

	public:
		void on_user_exchange_exception(long user, long handle) override;
		void on_user_exchange_resume(long user, long handle) override;
		void on_audio_exchange_exception(long user, long handle) override;
		void on_playback_exception(long user, long handle) override;
		void on_disk_format_exception(long user, long handle) override;
		void on_passive_decoding_exception(long user, long handle) override;
		void on_email_test_exception(long user, long handle) override;
		void on_backup_exception(long user, long handle) override;
		void on_network_flow_test_exception(long user, long handle) override;
		void on_push_mode_login_success(long user, long handle) override;

	public:
		void on_unknown_message(unsigned short type, long user, long handle) override;

	protected:
		virtual ~HikVisionExceptionHandler() noexcept {}
	};
}
