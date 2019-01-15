#include <cwchar>
#include <chrono>
#include <ctime>
#include <thread>
#include <Windows.h>
#include <timezoneapi.h>

#include "time.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

static const long long l00ns_us = 10LL;
static const long long l00ns_ms = l00ns_us * 1000LL;
static const long long l00ns_s = l00ns_ms * 1000LL;
static const long long ms_1601_1970 = 11644473600000LL;
static const long long l00ns_1601_1970 = ms_1601_1970 * l00ns_ms;

static inline void wtime(wchar_t* timestamp, time_t utc_s, const wchar_t* tfmt, bool locale = false) {
	struct tm now_s;

	if (locale) {
		localtime_s(&now_s, &utc_s);
	} else {
		gmtime_s(&now_s, &utc_s);
	}

	wcsftime(timestamp, 31, tfmt, &now_s);
}

static inline long long current_hectonanoseconds() {
	// Stupid Windows Machine
	FILETIME l00ns_1601;
	long long l00ns_1970 = 0LL;

	GetSystemTimeAsFileTime(&l00ns_1601);
	l00ns_1970 = ((long long)(l00ns_1601.dwHighDateTime) << 32) | l00ns_1601.dwLowDateTime;

	return l00ns_1970 - l00ns_1601_1970;
}

/**************************************************************************************************/
long long WarGrey::SCADA::floor_seconds(long long the_100ns, long long span) {
	long long the_second = the_100ns / l00ns_s;
	long long remainder = the_second % span;

	return the_second - remainder;
}

long long WarGrey::SCADA::ceiling_seconds(long long the_100ns, long long span) {
	long long the_second = the_100ns / l00ns_s;
	long long remainder = the_second % span;

	return the_second + (span - remainder);
}

long long WarGrey::SCADA::time_zone_utc_bias_seconds() {
	TIME_ZONE_INFORMATION tz;

	GetTimeZoneInformation(&tz);

	return tz.Bias * minute_span_s;
}

/**************************************************************************************************/
long long WarGrey::SCADA::current_100nanoseconds() {
	return current_hectonanoseconds();
}

long long WarGrey::SCADA::current_microseconds() {
	return current_hectonanoseconds() / l00ns_us;
}

long long WarGrey::SCADA::current_milliseconds() {
	return current_hectonanoseconds() / l00ns_ms;
}

double WarGrey::SCADA::current_inexact_milliseconds() {
	long long l00ns = current_hectonanoseconds();
	
	return double(l00ns / l00ns_ms) + double(l00ns % l00ns_ms) / double(l00ns_ms);
}

double WarGrey::SCADA::current_inexact_seconds() {
	long long l00ns = current_hectonanoseconds();

	return double(l00ns / l00ns_s) + double(l00ns % l00ns_s) / double(l00ns_s);
}

/**************************************************************************************************/
long long WarGrey::SCADA::current_seconds() {
	return current_hectonanoseconds() / l00ns_s;
}

long long WarGrey::SCADA::current_floor_seconds(long long span) {
	return floor_seconds(current_hectonanoseconds(), span);
}

long long WarGrey::SCADA::current_ceiling_seconds(long long span) {
	return ceiling_seconds(current_hectonanoseconds(), span);
}

/**************************************************************************************************/
TimeSpan WarGrey::SCADA::make_timespan_from_milliseconds(long long ms) {
	TimeSpan ts;
	
	ts.Duration = ms * l00ns_ms;

	return ts;
}

TimeSpan WarGrey::SCADA::make_timespan_from_seconds(long long s) {
	TimeSpan ts;

	ts.Duration = s * l00ns_s;

	return ts;
}

TimeSpan WarGrey::SCADA::make_timespan_from_rate(int rate, unsigned int shift) {
	TimeSpan ts;
	
	ts.Duration = ((rate > 0) ? (l00ns_s / rate) : (-l00ns_s * rate)) / max(shift, 1);
	
	return ts;
}

void WarGrey::SCADA::sleep(long long ms) {
	std::this_thread::sleep_for(std::chrono::milliseconds(ms));
}

void WarGrey::SCADA::sleep_us(long long us) {
	std::this_thread::sleep_for(std::chrono::microseconds(us));
}

/*************************************************************************************************/
void WarGrey::SCADA::split_date_utc(long long s, bool locale, long long* year, long long* month, long long* day) {
	struct tm datetime;

	if (locale) {
		localtime_s(&datetime, &s);
	} else {
		gmtime_s(&datetime, &s);
	}

	SET_BOX(year, datetime.tm_year + 1900);
	SET_BOX(month, datetime.tm_mon + 1);
	SET_BOX(day, datetime.tm_mday);
}

void WarGrey::SCADA::split_time_utc(long long s, bool locale, long long* hours, long long* minutes, long long* seconds) {
	long long daytime = (locale ? (s - time_zone_utc_bias_seconds()) : s) % day_span_s;
	
	SET_BOX(hours, daytime / hour_span_s);
	SET_BOX(minutes, daytime % hour_span_s / minute_span_s);
	SET_BOX(seconds, daytime % minute_span_s);
}

long long WarGrey::SCADA::seconds_add_seconds(long long s, long long count) {
	return s + count;
}

long long WarGrey::SCADA::seconds_add_minutes(long long s, long long count) {
	return s + minute_span_s * count;
}

long long WarGrey::SCADA::seconds_add_hours(long long s, long long count) {
	return s + hour_span_s * count;
}

long long WarGrey::SCADA::seconds_add_days(long long s, long long count) {
	return s + day_span_s * count;
}

long long WarGrey::SCADA::seconds_add_months(long long s, long long count) {
	struct tm datetime;

	gmtime_s(&datetime, &s);
	datetime.tm_mon += int(count);

	return _mkgmtime(&datetime);
}

long long WarGrey::SCADA::seconds_add_years(long long s, long long count) {
	struct tm datetime;

	gmtime_s(&datetime, &s);	
	datetime.tm_year += int(count);

	return _mkgmtime(&datetime);
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::make_timestamp_utc(long long utc_s, bool locale) {
	wchar_t timestamp[32];

	wtime(timestamp, utc_s, L"%F %T", locale);

	return ref new Platform::String(timestamp);
}

Platform::String^ WarGrey::SCADA::make_datestamp_utc(long long utc_s, bool locale) {
	wchar_t timestamp[32];

	wtime(timestamp, utc_s, L"%F", locale);

	return ref new Platform::String(timestamp);
}

Platform::String^ WarGrey::SCADA::make_daytimestamp_utc(long long utc_s, bool locale) {
	wchar_t timestamp[32];

	wtime(timestamp, utc_s, L"%T", locale);
		
	return ref new Platform::String(timestamp);
}

Platform::String^ WarGrey::SCADA::update_nowstamp(bool need_us, int* l00ns) {
	long long hecto_ns = current_hectonanoseconds();
	long long us = hecto_ns / 10L;
	Platform::String^ ts = make_timestamp_utc(hecto_ns / l00ns_s, true);
	
	if (need_us) {
		ts += ".";
		ts += (us % 1000000LL).ToString();
	}

	if (l00ns != nullptr) {
		(*l00ns) = hecto_ns % l00ns_s;
	}

	return ts;
}
