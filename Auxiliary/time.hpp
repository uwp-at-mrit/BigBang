#pragma once

namespace WarGrey::SCADA {
	static const long long minute_span_s = 60LL;
	static const long long hour_span_s = 60LL * minute_span_s;
	static const long long day_span_s = 24LL * hour_span_s;

	Windows::Foundation::TimeSpan make_timespan_from_milliseconds(long long ms);
	Windows::Foundation::TimeSpan make_timespan_from_seconds(long long s);
	Windows::Foundation::TimeSpan make_timespan_from_rate(int rate);

	long long floor_seconds(long long the_100ns, long long span_s = day_span_s);
	long long ceiling_seconds(long long the_100ns, long long span_s = day_span_s);
	long long time_zone_utc_bias_seconds();

	long long current_microseconds();
	long long current_milliseconds();
	double current_inexact_milliseconds();
	double current_inexact_seconds();

	long long current_seconds();
	long long current_100nanoseconds();
	long long current_floor_seconds(long long span_s = day_span_s);
	long long current_ceiling_seconds(long long span_s = day_span_s);

	void split_date_utc(long long s, bool locale, long long* year, long long* month, long long* day);
	void split_time_utc(long long s, bool locale, long long* hours, long long* minutes, long long* seconds);
	
	long long seconds_add_seconds(long long s, long long count);
	long long seconds_add_minutes(long long s, long long count);
	long long seconds_add_hours(long long s, long long count);
	long long seconds_add_days(long long s, long long count);
	long long seconds_add_months(long long s, long long count);
	long long seconds_add_years(long long s, long long count);

	void sleep(long long ms);
	void sleep_us(long long us);
	
	Platform::String^ make_timestamp_utc(long long utc_s, bool locale);
	Platform::String^ make_datestamp_utc(long long utc_s, bool locale);
	Platform::String^ make_daytimestamp_utc(long long utc_s, bool locale);

	Platform::String^ update_nowstamp(bool need_us = true, int* l00ns = nullptr);
}
