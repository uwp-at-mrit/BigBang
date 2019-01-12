#include <cstdio>

#include "diagnostics.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System::Diagnostics;

/*************************************************************************************************/
static ProcessCpuUsageReport^ process_cpu_usage() {
	static ProcessDiagnosticInfo^ self = nullptr;

	if (self == nullptr) {
		self = ProcessDiagnosticInfo::GetForCurrentProcess();
	}

	return self->CpuUsage->GetReport();
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
