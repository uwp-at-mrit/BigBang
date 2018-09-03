#pragma once

namespace WarGrey::SCADA {
	Windows::Foundation::TimeSpan make_timespan_from_ms(unsigned int ms);
	Windows::Foundation::TimeSpan make_timespan_from_rate(int rate);

	long long today_current_100ns();
	long long today_first_100ns();
	long long tomorrow_first_100ns();

	void sleep(unsigned int ms);

	wchar_t* make_wtimestamp(Windows::Globalization::Calendar^ c, bool need_us = true, int *l00ns = nullptr);
	Platform::String^ make_timestamp(Windows::Globalization::Calendar^ c, bool need_us = true, int* l00ns = nullptr);
	Platform::String^ make_timestamp(Windows::Foundation::DateTime& datetime, bool need_us = true, int* l00ns = nullptr);
	Platform::String^ make_timestamp(long long ut_l00ns, bool need_us = true, int* l00ns = nullptr);

	wchar_t* make_wdaytimestamp(Windows::Globalization::Calendar^ c, bool need_us = false, int *l00ns = nullptr);
	Platform::String^ make_daytimestamp(Windows::Globalization::Calendar^ c, bool need_us = false, int* l00ns = nullptr);
	Platform::String^ make_daytimestamp(Windows::Foundation::DateTime& datetime, bool need_us = false, int* l00ns = nullptr);
	Platform::String^ make_daytimestamp(long long ut_l00ns, bool need_us = false, int* l00ns = nullptr);

	Platform::String^ update_nowstamp(bool need_us = true, int* l00ns = nullptr);

	void process_usage(long long* kernel, long long* user);
	void process_usage_diff(long long* kernel, long long* user);
	Platform::String^ timing_string(long long kstart, long long ustart);

#define BEGIN_TIMING { long long _k_e_r_n_e_l_, _u_s_e_r_; process_usage(&_k_e_r_n_e_l_, &_u_s_e_r_);
#define END_TIMING(sendf) sendf(timing_string(_k_e_r_n_e_l_, _u_s_e_r_)); }
}
