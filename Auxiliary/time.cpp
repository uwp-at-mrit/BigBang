#include <cwchar>
#include <chrono>
#include <thread>

#include "time.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::System::Diagnostics;

#define STRFDAY(datetime) datetime->Hour, datetime->Minute, datetime->Second
#define STRFTIME(datetime) datetime->Year, datetime->Month, datetime->Day, STRFDAY(datetime)

static Calendar^ calendar = ref new Calendar();
static wchar_t timestamp[32];

static ProcessCpuUsageReport^ process_cpu_usage() {
    static ProcessDiagnosticInfo^ self = nullptr;

    if (self == nullptr) {
        self = ProcessDiagnosticInfo::GetForCurrentProcess();
    }

    return self->CpuUsage->GetReport();
}

inline static void mask_calendar_day(Calendar^ c) {
	calendar->Hour = 0;
	calendar->Minute = 0;
	calendar->Second = 0;
	calendar->Nanosecond = 0;
}

/**************************************************************************************************/
long long WarGrey::SCADA::today_first_100ns() {
	calendar->SetToNow();

	mask_calendar_day(calendar);

	return calendar->GetDateTime().UniversalTime;
}

long long WarGrey::SCADA::today_current_100ns() {
	calendar->SetToNow();

	return calendar->GetDateTime().UniversalTime;
}

long long WarGrey::SCADA::tomorrow_first_100ns() {
	calendar->SetToNow();

	mask_calendar_day(calendar);
	calendar->AddDays(1);

	return calendar->GetDateTime().UniversalTime;
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
wchar_t* WarGrey::SCADA::make_wtimestamp(Calendar^ c, bool need_us, int *l00nanosecond) {
	int l00ns = c->Nanosecond / 100;
	
	if (need_us) {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d.%06d", STRFTIME(c), l00ns / 10);
	} else {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d", STRFTIME(c));
	}

	SET_BOX(l00nanosecond, l00ns);

	return timestamp;
}

Platform::String^ WarGrey::SCADA::make_timestamp(Calendar^ c, bool need_us, int* l00ns) {
	return ref new Platform::String(make_wtimestamp(c, need_us, l00ns));
}

Platform::String^ WarGrey::SCADA::make_timestamp(DateTime& dt, bool need_us, int* l00ns) {
	calendar->SetDateTime(dt);

	return ref new Platform::String(make_wtimestamp(calendar, need_us, l00ns));
}

Platform::String^ WarGrey::SCADA::make_timestamp(long long ut_l00ns, bool need_us, int* l00ns) {
	DateTime dt;

	dt.UniversalTime = ut_l00ns;

	return make_timestamp(dt, need_us, l00ns);
}

wchar_t* WarGrey::SCADA::make_wdaytimestamp(Calendar^ c, bool need_us, int *l00nanosecond) {
	int l00ns = c->Nanosecond / 100;
	
	if (need_us) {
		swprintf(timestamp, 31, L"%02d:%02d:%02d.%06d", STRFDAY(c), l00ns / 10);
	} else {
		swprintf(timestamp, 31, L"%02d:%02d:%02d", STRFDAY(c));
	}

	SET_BOX(l00nanosecond, l00ns);

	return timestamp;
}

Platform::String^ WarGrey::SCADA::make_daytimestamp(Calendar^ c, bool need_us, int* l00ns) {
	return ref new Platform::String(make_wdaytimestamp(c, need_us, l00ns));
}

Platform::String^ WarGrey::SCADA::make_daytimestamp(DateTime& dt, bool need_us, int* l00ns) {
	calendar->SetDateTime(dt);

	return ref new Platform::String(make_wdaytimestamp(calendar, need_us, l00ns));
}

Platform::String^ WarGrey::SCADA::make_daytimestamp(long long ut_l00ns, bool need_us, int* l00ns) {
	DateTime dt;

	dt.UniversalTime = ut_l00ns;

	return make_daytimestamp(dt, need_us, l00ns);
}

Platform::String^ WarGrey::SCADA::update_nowstamp(bool need_us, int* l00ns) {
	calendar->SetToNow();
	
	return ref new Platform::String(make_wtimestamp(calendar, need_us, l00ns));
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
