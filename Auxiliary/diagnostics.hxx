#pragma once

namespace WarGrey::SCADA {
	void process_usage(long long* kernel, long long* user);
	void process_usage_diff(long long* kernel, long long* user);
	Platform::String^ timing_string(long long kstart, long long ustart);

#define BEGIN_TIMING { long long _k_e_r_n_e_l_, _u_s_e_r_; process_usage(&_k_e_r_n_e_l_, &_u_s_e_r_);
#define END_TIMING(sendf, log_level) sendf(log_level, timing_string(_k_e_r_n_e_l_, _u_s_e_r_)); }
}
