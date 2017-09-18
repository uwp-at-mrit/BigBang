#include <ppltasks.h>

#include "rsyslog.hpp"

using namespace Concurrency;

using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

using namespace Windows::Globalization;
using namespace Windows::Globalization::DateTimeFormatting;

#define DEFAULT_POOL_SIZE 2048
static wchar_t pool[DEFAULT_POOL_SIZE];

static Calendar^ datetime = nullptr;
static DatagramSocket^ client = nullptr;
static IDataWriter^ udpout = nullptr;

static void syslog(Platform::String^ message) {
    if (client == nullptr) {
        auto loghost = ref new HostName("172.16.8.1");
        
        datetime = ref new Calendar();
        client = ref new DatagramSocket();

        create_task(client->ConnectAsync(loghost, "18030")).then([message](task<void> conn) {
            conn.get();
            udpout = ref new DataWriter(client->OutputStream);
            syslog(message);
        });
    } else if (udpout != nullptr) {
        datetime->SetToNow();
        long long ms = datetime->Nanosecond / 1000000;
        auto record = L"[" + datetime->YearAsPaddedString(4)
            + L"-" + datetime->MonthAsPaddedNumericString(2)
            + L"-" + datetime->DayAsPaddedString(2)
            + L" " + datetime->HourAsPaddedString(2)
            + L":" + datetime->MinuteAsPaddedString(2)
            + L":" + datetime->SecondAsPaddedString(2)
            + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : ".")) + ms.ToString()
            + L"] " + message;

        udpout->WriteString(record);
        udpout->StoreAsync();
        
        OutputDebugString(record->Data());
        OutputDebugString(L"\n");
    }
}

void rsyslog(Platform::String^ message) {
    syslog(message);
}

void rsyslog(const wchar_t *fmt, ...) {
    va_list argl;
    va_start(argl, fmt);
    vswprintf(pool, DEFAULT_POOL_SIZE, fmt, argl);
    va_end(argl);

    rsyslog(ref new Platform::String(pool));
}
