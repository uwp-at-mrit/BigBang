#include <cwchar>
#include <chrono>
#include <ctime>
#include <thread>

#include "time.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::System::Diagnostics;

static Calendar^ calendar = ref new Calendar();
static struct tm now_s;
static wchar_t timestamp[32];

static wchar_t* wtime(time_t utc_s, const wchar_t* tfmt, bool locale = false) {
	if (locale) {
		localtime_s(&now_s, &utc_s);
	} else {
		gmtime_s(&now_s, &utc_s);
	}

	wcsftime(timestamp, 31, tfmt, &now_s);

	return timestamp;
}

static ProcessCpuUsageReport^ process_cpu_usage() {
    static ProcessDiagnosticInfo^ self = nullptr;

    if (self == nullptr) {
        self = ProcessDiagnosticInfo::GetForCurrentProcess();
    }

    return self->CpuUsage->GetReport();
}

/**************************************************************************************************/
long long WarGrey::SCADA::current_seconds() {
	return std::time(nullptr);
}

long long WarGrey::SCADA::current_floor_seconds(long long span) {
	long long now = current_seconds();
	long long remainder = now % span;

	return now - remainder;
}

long long WarGrey::SCADA::current_ceiling_seconds(long long span) {
	long long now = current_seconds();
	long long remainder = now % span;

	return now + (span - remainder);
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
Platform::String^ WarGrey::SCADA::make_timestamp(long long utc_s, bool locale) {
	return ref new Platform::String(wtime(utc_s, L"%Y-%m-%d %H:%M:%S", locale));
}

Platform::String^ WarGrey::SCADA::make_daytimestamp(long long utc_s, bool locale) {
	return ref new Platform::String(wtime(utc_s, L"%H:%M:%S", locale));
}

Platform::String^ WarGrey::SCADA::update_nowstamp(bool need_us, int* l00ns) {
	Platform::String^ ts = make_timestamp(current_seconds(), true);

	if (need_us || (l00ns != nullptr)) {
		calendar->SetToNow();
	}

	if (need_us) {
		ts += (calendar->Nanosecond / 10LL / 1000LL).ToString();
	}

	if (l00ns != nullptr) {
		(*l00ns) = calendar->Nanosecond;
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
