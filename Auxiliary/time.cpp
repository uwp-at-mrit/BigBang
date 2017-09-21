#include <cwchar>
#include "time.hpp"

using namespace Windows::Globalization;

wchar_t* update_wnowstamp(int* l00nanoseconds) {
    static Calendar^ datetime = ref new Calendar();
    static wchar_t timestamp[32];

    datetime->SetToNow();
    int l00ns = datetime->Nanosecond / 100;
    int ms = l00ns / 10000;
    swprintf(timestamp, 31, L"%d-%s-%s %s:%s:%s.%s%ld", datetime->Year,
        datetime->MonthAsPaddedNumericString(2)->Data(), datetime->DayAsPaddedString(2)->Data(),
        datetime->HourAsPaddedString(2)->Data(), datetime->MinuteAsPaddedString(2)->Data(),
        datetime->SecondAsPaddedString(2)->Data(),
        ((ms < 10) ? L"00" : ((ms < 100) ? L"0" : L"")), ms);

    if (l00nanoseconds != nullptr) (*l00nanoseconds) = l00ns;
    return timestamp;
}


Platform::String^ update_nowstamp(int* l00ns) {
    return ref new Platform::String(update_wnowstamp(l00ns));
}
