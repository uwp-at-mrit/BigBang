#include "hv/hcnet.hpp"

#include "usr/share/include/HCNetSDK.h"
#include "string.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

static inline bool fill_local_cfg(HikVision* master, const char* prefix, NET_SDK_LOCAL_CFG_TYPE type, void* out) {
	bool okay = NET_DVR_GetSDKLocalCfg(type, out);

	if (!okay) {
		master->report_warning("get %s configuration", prefix);
	}

	return okay;
}

static inline bool configure_local_cfg(HikVision* master, const char* prefix, NET_SDK_LOCAL_CFG_TYPE type, void* out) {
	bool okay = NET_DVR_SetSDKLocalCfg(type, out);

	if (!okay) {
		master->report_warning("set %s configuration", prefix);
	}

	return okay;
}

/*************************************************************************************************/
static void hv_exception_callback(unsigned long type, long user, long handle, void* urgent) {
	if (urgent != nullptr) {
		IHikVisionExceptionHandler* e = static_cast<IHikVisionExceptionHandler*>(urgent);

		switch (type) {
		case EXCEPTION_EXCHANGE: e->on_user_exchange_exception(user, handle); break;
		case EXCEPTION_AUDIOEXCHANGE: e->on_audio_exchange_exception(user, handle); break;
		case EXCEPTION_ALARM: e->on_alarm_message(user, handle, HikVisionAlarmMsg::Exception); break;
		case EXCEPTION_PREVIEW: e->on_preview_message(user, handle, HikVisionMsg::Exception); break;
		case EXCEPTION_SERIAL: e->on_channel_message(user, handle, HikVisionMsg::Exception); break;
		case EXCEPTION_RECONNECT: e->on_preview_message(user, handle, HikVisionMsg::Reconnection); break;
		case EXCEPTION_ALARMRECONNECT: e->on_alarm_message(user, handle, HikVisionAlarmMsg::Reconnection); break;
		case EXCEPTION_SERIALRECONNECT: e->on_channel_message(user, handle, HikVisionMsg::Reconnection); break;
		case SERIAL_RECONNECTSUCCESS: e->on_channel_message(user, handle, HikVisionMsg::Success); break;
		case EXCEPTION_PLAYBACK: e->on_playback_exception(user, handle); break;
		case EXCEPTION_DISKFMT: e->on_disk_format_exception(user, handle); break;
		case EXCEPTION_PASSIVEDECODE: e->on_passive_decoding_exception(user, handle); break;
		case EXCEPTION_EMAILTEST: e->on_email_test_exception(user, handle); break;
		case EXCEPTION_BACKUP: e->on_backup_exception(user, handle); break;
		case PREVIEW_RECONNECTSUCCESS: e->on_preview_message(user, handle, HikVisionMsg::Success);
		case ALARM_RECONNECTSUCCESS: e->on_alarm_message(user, handle, HikVisionAlarmMsg::Success);
		case RESUME_EXCHANGE: e->on_user_exchange_resume(user, handle); break;
		case NETWORK_FLOWTEST_EXCEPTION: e->on_network_flow_test_exception(user, handle); break;
		}
	}
}

/*************************************************************************************************/
static Syslog* hv_logger = nullptr;
static HikVisionNet* self = nullptr;
static bool hv_configure_okay = true;

void HikVisionNet::configure(Syslog* logger) {
	hv_logger = logger;
}

void HikVisionNet::configure(HikVisionCfg malarm, HikVisionCfg muser) {
	static NET_DVR_INIT_CFG_ABILITY cfg;

	cfg.enumMaxAlarmNum = INIT_CFG_MAX_NUM(static_cast<unsigned int>(malarm));
	cfg.enumMaxLoginUsersNum = INIT_CFG_MAX_NUM(static_cast<unsigned int>(muser));

	hv_configure_okay = NET_DVR_SetSDKInitCfg(NET_SDK_INIT_CFG_ABILITY, &cfg);
}

HikVisionNet* HikVisionNet::instace() {
	if (self == nullptr) {
		self = new HikVisionNet();
	}

	return self;
}

void HikVisionNet::cleanup() {
	delete self;

	self = nullptr;
	hv_configure_okay = true;
}

/*************************************************************************************************/
HikVisionNet::HikVisionNet() : HikVision(hv_logger) {
	if (!hv_configure_okay) {
		this->report_warning("configure");
	}

	if (!NET_DVR_Init()) {
		this->report_error("initialize");
	}
}

HikVisionNet::~HikVisionNet() {
	if (!NET_DVR_Cleanup()) {
		this->report_warning("cleanup");
	}
}

unsigned long HikVisionNet::version(bool needs_build_number) {
	if (needs_build_number) {
		return NET_DVR_GetSDKBuildVersion();
	} else {
		return NET_DVR_GetSDKVersion();
	}
}

HikVisionMatrics HikVisionNet::statistics() {
	HikVisionMatrics stats;
	NET_DVR_SDKSTATE src;

	if (this->report_on_warning(NET_DVR_GetSDKState(&src), "GetSDKState")) {
		memcpy(&stats, &src, sizeof(stats));
		//stats.login = src.dwTotalLoginNum;
		//stats.real_play = src.dwTotalRealPlayNum;
		//stats.playback = src.dwTotalPlayBackNum;
		//stats.alarm_chan = src.dwTotalAlarmChanNum;
		//stats.format = src.dwTotalFormatNum;
		//stats.file_search = src.dwTotalFileSearchNum;
		//stats.log_search = src.dwTotalLogSearchNum;
		//stats.serial = src.dwTotalSerialNum;
		//stats.upgrade = src.dwTotalUpgradeNum;
		//stats.voice_com = src.dwTotalVoiceComNum;
		//stats.broadcast = src.dwTotalBroadCastNum;
	}

	return stats;
}

HikVisionMatrics HikVisionNet::abilities() {
	HikVisionMatrics stats;
	NET_DVR_SDKABL src;

	if (this->report_on_warning(NET_DVR_GetSDKAbility(&src), "GetSDKAbility")) {
		memcpy(&stats, &src, sizeof(stats));
		//stats.login = src.dwMaxLoginNum;
		//stats.real_play = src.dwMaxRealPlayNum;
		//stats.playback = src.dwMaxPlayBackNum;
		//stats.alarm_chan = src.dwMaxAlarmChanNum;
		//stats.format = src.dwMaxFormatNum;
		//stats.file_search = src.dwMaxFileSearchNum;
		//stats.log_search = src.dwMaxLogSearchNum;
		//stats.serial = src.dwMaxSerialNum;
		//stats.upgrade = src.dwMaxUpgradeNum;
		//stats.voice_com = src.dwMaxVoiceComNum;
		//stats.broadcast = src.dwMaxBroadCastNum;
	}

	return stats;
}

std::list<std::string> HikVisionNet::get_local_ips(bool* enable_bind) {
	std::list<std::string> ips;
	char pools[16][16];
	unsigned long count;
	BOOL enabled;

	if (this->report_on_warning(NET_DVR_GetLocalIP(pools, &count, &enabled), "GetLocalIP")) {
		for (unsigned long idx = 0; idx < count; idx++) {
			ips.push_back(std::string(pools[idx]));
		}

		SET_BOX(enable_bind, enabled != 0);
	}

	return ips;
}

bool HikVisionNet::set_valid_ip(unsigned long index, bool enable_bind) {
	return this->report_on_error(NET_DVR_SetValidIP(index, enable_bind), "SetValidIP");
}

bool HikVisionNet::set_connection_time(unsigned long wait_time, unsigned long try_times) {
	return this->report_on_warning(NET_DVR_SetConnectTime(wait_time, try_times), "SetConnectTime");
}

bool HikVisionNet::set_receive_timeout(unsigned long timeout) {
	return this->report_on_warning(NET_DVR_SetRecvTimeOut(timeout), "SetRecvTimeout");
}

bool HikVisionNet::set_reconnect_time(unsigned long interval, bool enable) {
	return this->report_on_warning(NET_DVR_SetReconnect(interval, (enable ? 1 : 0)), "SetReconnectTime");
}

bool HikVisionNet::set_exception_handler(IHikVisionExceptionHandler* exn_callback) {
	//bool okay = NET_DVR_SetExceptionCallBack_V30(WM_NULL, nullptr, hv_exception_callback, exn_callback);

	//if (!okay) {
	//	this->report_warning("SetExceptionCallback");
	//}

	//return okay;
	return false;
}

bool HikVisionNet::tcp_port_cfg(unsigned short* min_port, unsigned short* max_port) {
	static NET_DVR_LOCAL_TCP_PORT_BIND_CFG cfg;
	bool okay = fill_local_cfg(this, "tcp", NET_SDK_LOCAL_CFG_TYPE_TCP_PORT_BIND, &cfg);

	SET_BOX(min_port, cfg.wLocalBindTcpMinPort);
	SET_BOX(max_port, cfg.wLocalBindTcpMaxPort);

	return okay;
}

bool HikVisionNet::tcp_port_cfg(unsigned short min_port, unsigned short max_port) {
	static NET_DVR_LOCAL_TCP_PORT_BIND_CFG cfg;

	cfg.wLocalBindTcpMinPort = min_port;
	cfg.wLocalBindTcpMaxPort = max_port;

	return configure_local_cfg(this, "tcp", NET_SDK_LOCAL_CFG_TYPE_TCP_PORT_BIND, &cfg);
}

bool HikVisionNet::udp_port_cfg(unsigned short* min_port, unsigned short* max_port) {
	static NET_DVR_LOCAL_UDP_PORT_BIND_CFG cfg;
	bool okay = fill_local_cfg(this, "udp", NET_SDK_LOCAL_CFG_TYPE_UDP_PORT_BIND, &cfg);

	SET_BOX(min_port, cfg.wLocalBindUdpMinPort);
	SET_BOX(max_port, cfg.wLocalBindUdpMaxPort);

	return okay;
}

bool HikVisionNet::udp_port_cfg(unsigned short min_port, unsigned short max_port) {
	static NET_DVR_LOCAL_UDP_PORT_BIND_CFG cfg;

	cfg.wLocalBindUdpMinPort = min_port;
	cfg.wLocalBindUdpMaxPort = max_port;

	return configure_local_cfg(this, "tcp", NET_SDK_LOCAL_CFG_TYPE_UDP_PORT_BIND, &cfg);
}

bool HikVisionNet::memory_pool_cfg(unsigned long* max_block, unsigned long* alarm_interval, unsigned long* object_interval) {
	static NET_DVR_LOCAL_MEM_POOL_CFG cfg;
	bool okay = fill_local_cfg(this, "memory pool", NET_SDK_LOCAL_CFG_TYPE_MEM_POOL, &cfg);

	SET_BOX(max_block, cfg.dwAlarmMaxBlockNum);
	SET_BOX(alarm_interval, cfg.dwAlarmReleaseInterval);
	SET_BOX(object_interval, cfg.dwObjectReleaseInterval);

	return okay;
}

bool HikVisionNet::memory_pool_cfg(unsigned long max_block, unsigned long alarm_interval, unsigned long object_interval) {
	static NET_DVR_LOCAL_MEM_POOL_CFG cfg;

	cfg.dwAlarmMaxBlockNum = max_block;
	cfg.dwAlarmReleaseInterval = alarm_interval;
	cfg.dwObjectReleaseInterval = object_interval;

	return configure_local_cfg(this, "memory pool", NET_SDK_LOCAL_CFG_TYPE_MEM_POOL, &cfg);
}

bool HikVisionNet::module_timeout_cfg(unsigned long* prev, unsigned long* alarm, unsigned long* vod, unsigned long* otherwise) {
	static NET_DVR_LOCAL_MODULE_RECV_TIMEOUT_CFG cfg;
	bool okay = fill_local_cfg(this, "module timeout", NET_SDK_LOCAL_CFG_TYPE_MODULE_RECV_TIMEOUT, &cfg);

	SET_BOX(prev, cfg.dwPreviewTime);
	SET_BOX(alarm, cfg.dwAlarmTime);
	SET_BOX(vod, cfg.dwVodTime);
	SET_BOX(otherwise, cfg.dwElse);

	return okay;
}

bool HikVisionNet::module_timeout_cfg(unsigned long prev, unsigned long alarm, unsigned long vod, unsigned long otherwise) {
	static NET_DVR_LOCAL_MODULE_RECV_TIMEOUT_CFG cfg;

	cfg.dwPreviewTime = prev;
	cfg.dwAlarmTime = alarm;
	cfg.dwVodTime = vod;
	cfg.dwElse = otherwise;

	return configure_local_cfg(this, "module timeout", NET_SDK_LOCAL_CFG_TYPE_MODULE_RECV_TIMEOUT, &cfg);
}

bool HikVisionNet::ability_parse_cfg(bool* enabled) {
	static NET_DVR_LOCAL_ABILITY_PARSE_CFG cfg;
	bool okay = fill_local_cfg(this, "ability parse", NET_SDK_LOCAL_CFG_TYPE_ABILITY_PARSE, &cfg);

	SET_BOX(enabled, cfg.byEnableAbilityParse != 0);

	return okay;
}

bool HikVisionNet::ability_parse_cfg(bool enabled) {
	static NET_DVR_LOCAL_ABILITY_PARSE_CFG cfg;

	cfg.byEnableAbilityParse = (enabled ? 1 : 0);

	return configure_local_cfg(this, "ability parse", NET_SDK_LOCAL_CFG_TYPE_TCP_PORT_BIND, &cfg);
}

bool HikVisionNet::talk_mode_cfg(bool* enabled) {
	static NET_DVR_LOCAL_TALK_MODE_CFG cfg;
	bool okay = fill_local_cfg(this, "talk mode", NET_SDK_LOCAL_CFG_TYPE_TALK_MODE, &cfg);

	SET_BOX(enabled, cfg.byTalkMode != 0);

	return okay;
}

bool HikVisionNet::talk_mode_cfg(bool enabled) {
	static NET_DVR_LOCAL_TALK_MODE_CFG cfg;

	cfg.byTalkMode = (enabled ? 1 : 0);

	return configure_local_cfg(this, "talk mode", NET_SDK_LOCAL_CFG_TYPE_TALK_MODE, &cfg);
}

bool HikVisionNet::check_device_cfg(unsigned long* timeout, unsigned long* max_failures) {
	static NET_DVR_LOCAL_CHECK_DEV cfg;
	bool okay = fill_local_cfg(this, "check device", NET_SDK_LOCAL_CFG_TYPE_CHECK_DEV, &cfg);

	SET_BOX(timeout, cfg.dwCheckOnlineTimeout);
	SET_BOX(max_failures, cfg.dwCheckOnlineNetFailMax);
	
	return okay;
}

bool HikVisionNet::check_device_cfg(unsigned long timeout, unsigned long max_failures) {
	static NET_DVR_LOCAL_CHECK_DEV cfg;

	cfg.dwCheckOnlineTimeout = timeout;
	cfg.dwCheckOnlineNetFailMax = max_failures;

	return configure_local_cfg(this, "check device", NET_SDK_LOCAL_CFG_TYPE_CHECK_DEV, &cfg);
}

bool HikVisionNet::general_cfg(bool* exn_cb_directly, bool* not_split_record_file, bool* resumable, uint64* filesize, unsigned long* resume_timeout) {
	static NET_DVR_LOCAL_GENERAL_CFG cfg;
	bool okay = fill_local_cfg(this, "general", NET_DVR_LOCAL_CFG_TYPE_GENERAL, &cfg);

	SET_BOX(exn_cb_directly, cfg.byExceptionCbDirectly != 0);
	SET_BOX(not_split_record_file, cfg.byNotSplitRecordFile != 0);
	SET_BOX(resumable, cfg.byResumeUpgradeEnable != 0);
	SET_BOX(filesize, cfg.i64FileSize);
	SET_BOX(resume_timeout, cfg.dwResumeUpgradeTimeout);

	return okay;
}

bool HikVisionNet::general_cfg(bool exn_cb_directly, bool not_split_record_file, bool resumable, uint64 filesize, unsigned long resume_timeout) {
	static NET_DVR_LOCAL_GENERAL_CFG cfg;

	cfg.byExceptionCbDirectly = (exn_cb_directly ? 1 : 0);
	cfg.byNotSplitRecordFile = (not_split_record_file ? 1 : 0);
	cfg.byResumeUpgradeEnable = (resumable ? 1 : 0);
	cfg.i64FileSize = filesize;
	cfg.dwResumeUpgradeTimeout = resume_timeout;

	return configure_local_cfg(this, "ability parse", NET_SDK_LOCAL_CFG_TYPE_TCP_PORT_BIND, &cfg);
}

bool HikVisionNet::char_encode_cfg(hv_chars_convert_f converter) {
	static NET_DVR_LOCAL_BYTE_ENCODE_CONVERT cfg;

	cfg.fnCharConvertCallBack = (CHAR_ENCODE_CONVERT)converter;

	return configure_local_cfg(this, "char encode", NET_SDK_LOCAL_CFG_TYPE_CHAR_ENCODE, &cfg);
}

bool HikVisionNet::log_cfg(unsigned short log_number) {
	static NET_DVR_LOCAL_LOG_CFG cfg;

	cfg.wSDKLogNum = log_number;
	
	return configure_local_cfg(this, "log", NET_DVR_LOCAL_CFG_TYPE_LOG, &cfg);
}