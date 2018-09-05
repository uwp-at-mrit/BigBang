#pragma once

namespace WarGrey::SCADA {
	static const long long minute_span_100ns = 60LL * 1000LL * 1000LL * 10LL;
	static const long long hour_span_100ns = 60LL * minute_span_100ns;
	static const long long day_span_100ns = 24LL * hour_span_100ns;

	Windows::Foundation::TimeSpan make_timespan_from_ms(unsigned int ms);
	Windows::Foundation::TimeSpan make_timespan_from_rate(int rate);

	long long this_moment_100ns();

	long long this_minute_first_100ns();
	long long next_minute_first_100ns();

	long long this_hour_first_100ns();
	long long next_hour_first_100ns();

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
