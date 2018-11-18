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
using namespace Windows::System::Diagnostics;

static const long long l00ns_ms = 1000LL * 10LL;
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

static ProcessCpuUsageReport^ process_cpu_usage() {
    static ProcessDiagnosticInfo^ self = nullptr;

    if (self == nullptr) {
        self = ProcessDiagnosticInfo::GetForCurrentProcess();
    }

    return self->CpuUsage->GetReport();
}

/**************************************************************************************************/
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
	long long now = current_hectonanoseconds() / l00ns_s;
	long long remainder = now % span;

	return now - remainder;
}

long long WarGrey::SCADA::current_ceiling_seconds(long long span) {
	long long now = current_hectonanoseconds() / l00ns_s;
	long long remainder = now % span;

	return now + (span - remainder);
}

long long WarGrey::SCADA::time_zone_utc_bias_seconds() {
	TIME_ZONE_INFORMATION tz;

	GetTimeZoneInformation(&tz);

	return tz.Bias * minute_span_s;
}

/**************************************************************************************************/
TimeSpan WarGrey::SCADA::make_timespan_from_ms(unsigned int ms) {
	TimeSpan ts;
	
	ts.Duration = ms * 10000LL;

	return ts;
}

TimeSpan WarGrey::SCADA::make_timespan_from_rate(int rate) {
	TimeSpan ts;
	long long l00ns = 10000000LL;
	
	ts.Duration = ((rate > 0) ? (l00ns / rate) : (-l00ns * rate));
	
	return ts;
}

void WarGrey::SCADA::sleep(unsigned int ms) {
	std::this_thread::sleep_for(std::chrono::milliseconds(ms));
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::make_timestamp_utc(long long utc_s, bool locale) {
	wchar_t timestamp[32];

	wtime(timestamp, utc_s, L"%F %T", locale);

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

/*************************************************************************************************/
void WarGrey::SCADA::process_usage(long long* kernel, long long* user) {
    ProcessCpuUsageReport^ now = process_cpu_usage();
    
    (*kernel) = now->KernelTime.Duration;
    (*user) = now->UserTime.Duration;
}

void WarGrey::SCADA::process_usage_diff(long long* kernel, long long* user) {
    ProcessCpuUsageReport^ now = process_cpu_usage();

    (*kernel) = now->KernelTime.Duration - (*kernel);
    (*user) = now->UserTime.Duration - (*user);
}

Platform::String^ WarGrey::SCADA::timing_string(long long kernel, long long user) {
    static wchar_t benchmark[32];
    
    process_usage_diff(&kernel, &user);

    auto kms = kernel / 10000LL;
    auto ums = user / 10000LL;
    swprintf(benchmark, 31, L"time: %lldms(%lldms + %lldms)", kms + ums, kms, ums);

    return ref new Platform::String(benchmark);
}
