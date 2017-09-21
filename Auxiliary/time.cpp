#include <cwchar>
#include "time.hpp"

using namespace Windows::Globalization;

#define STRFTIME(datetime) \
datetime->Year, \
datetime->MonthAsPaddedNumericString(2)->Data(), \
datetime->DayAsPaddedString(2)->Data(), \
datetime->HourAsPaddedString(2)->Data(), \
datetime->MinuteAsPaddedString(2)->Data(), \
datetime->SecondAsPaddedString(2)->Data()

wchar_t* update_wnowstamp(bool need_ms, int* l00nanosecond) {
    static Calendar^ datetime = ref new Calendar();
    static wchar_t timestamp[32];

    datetime->SetToNow();
    int l00ns = datetime->Nanosecond / 100;
    if (need_ms) {
        int ms = l00ns / 10000;
        wchar_t* prefix = ((ms < 10) ? L"00" : ((ms < 100) ? L"0" : L""));
        swprintf(timestamp, 31, L"%d-%s-%s %s:%s:%s.%s%ld", STRFTIME(datetime), prefix, ms);
    } else {
        swprintf(timestamp, 31, L"%d-%s-%s %s:%s:%s", STRFTIME(datetime));
    }

    if (l00nanosecond != nullptr) (*l00nanosecond) = l00ns;
    return timestamp;
}


Platform::String^ update_nowstamp(bool need_ms, int* l00ns) {
    return ref new Platform::String(update_wnowstamp(need_ms, l00ns));
}
