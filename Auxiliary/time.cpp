#include <cwchar>
#include <chrono>
#include <thread>

#include "time.hpp"
#include "box.hpp"

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::System::Diagnostics;

#define STRFTIME(datetime) \
    datetime->Year, datetime->Month, datetime->Day, \
    datetime->Hour, datetime->Minute, datetime->Second

static ProcessCpuUsageReport^ process_cpu_usage() {
    static ProcessDiagnosticInfo^ self = nullptr;

    if (self == nullptr) {
        self = ProcessDiagnosticInfo::GetForCurrentProcess();
    }

    return self->CpuUsage->GetReport();
}

TimeSpan make_timespan_from_ms(unsigned int ms) {
	TimeSpan ts;
	
	ts.Duration = ms * 10000LL;

	return ts;
}

TimeSpan make_timespan_from_rate(int rate) {
	TimeSpan ts;
	long long l00ns = 10000000LL;
	
	ts.Duration = ((rate > 0) ? (l00ns / rate) : (-l00ns * rate));
	
	return ts;
}

void sleep(unsigned int ms) {
	std::this_thread::sleep_for(std::chrono::milliseconds(ms));
}

wchar_t* make_wtimestamp(Calendar^ c, bool need_us, int *l00nanosecond) {
	static wchar_t timestamp[32];

	int l00ns = c->Nanosecond / 100;
	if (need_us) {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d.%06d", STRFTIME(c), l00ns / 10);
	} else {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d", STRFTIME(c));
	}

	SET_BOX(l00nanosecond, l00ns);

	return timestamp;
}

Platform::String^ make_timestamp(Calendar^ c, bool need_us, int* l00ns) {
	return ref new Platform::String(make_wtimestamp(c, need_us, l00ns));
}

Platform::String^ make_timestamp(Windows::Foundation::DateTime& dt, bool need_us, int* l00ns) {
	static Calendar^ c = ref new Calendar();

	c->SetDateTime(dt);

	return ref new Platform::String(make_wtimestamp(c, need_us, l00ns));
}

Platform::String^ update_nowstamp(bool need_us, int* l00ns) {
	static Calendar^ c = ref new Calendar();

	c->SetToNow();
	
	return ref new Platform::String(make_wtimestamp(c, need_us, l00ns));
}

void process_usage(long long* kernel, long long* user) {
    ProcessCpuUsageReport^ now = process_cpu_usage();
    
    (*kernel) = now->KernelTime.Duration;
    (*user) = now->UserTime.Duration;
}

void process_usage_diff(long long* kernel, long long* user) {
    ProcessCpuUsageReport^ now = process_cpu_usage();

    (*kernel) = now->KernelTime.Duration - (*kernel);
    (*user) = now->UserTime.Duration - (*user);
}

Platform::String^ timing_string(long long kernel, long long user) {
    static wchar_t benchmark[32];
    
    process_usage_diff(&kernel, &user);

    auto kms = kernel / 10000LL;
    auto ums = user / 10000LL;
    swprintf(benchmark, 31, L"time: %lldms(%lldms + %lldms)", kms + ums, kms, ums);

    return ref new Platform::String(benchmark);
}
