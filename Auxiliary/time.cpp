#include <cwchar>

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
	long long l00ns = ms * 10000LL;

	return TimeSpan({ l00ns });
}

TimeSpan make_timespan_from_rate(int rate) {
	long long l00ns = ((rate > 0) ? (10000000LL / rate) : (-10000000LL * rate));
	
	return TimeSpan({ l00ns });
}

wchar_t* update_wnowstamp(bool need_us, int* l00nanosecond) {
	static Calendar^ datetime = ref new Calendar();
    static wchar_t timestamp[32];

	datetime->SetToNow();
	int l00ns = datetime->Nanosecond / 100;
	if (need_us) {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d.%06d", STRFTIME(datetime), l00ns / 10);
	} else {
		swprintf(timestamp, 31, L"%d-%02d-%02d %02d:%02d:%02d", STRFTIME(datetime));
	}

	SET_BOX(l00nanosecond, l00ns);
	return timestamp;
}

Platform::String^ update_nowstamp(bool need_us, int* l00ns) {
    return ref new Platform::String(update_wnowstamp(need_us, l00ns));
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

    auto kms = kernel / 10000;
    auto ums = user / 10000;
    swprintf(benchmark, 31, L"time: %lldms(%lldms + %lldms)", kms + ums, kms, ums);

    return ref new Platform::String(benchmark);
}
