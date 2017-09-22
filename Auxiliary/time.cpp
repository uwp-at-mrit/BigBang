#include <cwchar>
#include "time.hpp"

using namespace Windows::Globalization;

#define STRFTIME(datetime) \
datetime->Year, datetime->Month, datetime->Day, \
datetime->Hour, datetime->Minute, datetime->Second

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

    if (l00nanosecond != nullptr) (*l00nanosecond) = l00ns;
    return timestamp;
}


Platform::String^ update_nowstamp(bool need_us, int* l00ns) {
    return ref new Platform::String(update_wnowstamp(need_us, l00ns));
}
