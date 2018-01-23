#pragma once

Windows::Foundation::TimeSpan make_timespan_from_seconds(unsigned int seconds);
Windows::Foundation::TimeSpan make_timespan_from_rate(int rate);

wchar_t* update_wnowstamp(bool need_us = true, int *l00ns = nullptr);
Platform::String^ update_nowstamp(bool need_us = true, int* l00ns = nullptr);

void process_usage(long long* kernel, long long* user);
void process_usage_diff(long long* kernel, long long* user);
Platform::String^ timing_string(long long kstart, long long ustart);

#define BEGIN_TIMING { long long _k_e_r_n_e_l_, _u_s_e_r_; process_usage(&_k_e_r_n_e_l_, &_u_s_e_r_);
#define END_TIMING(sendf) sendf(timing_string(_k_e_r_n_e_l_, _u_s_e_r_)); }
