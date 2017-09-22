#include <ppltasks.h>

#include "rsyslog.hpp"
#include "time.hpp"

using namespace Concurrency;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static DatagramSocket^ client = nullptr;
static IDataWriter^ udpout = nullptr;

static void trace(const wchar_t* message) {
    OutputDebugString(message);
    OutputDebugString(L"\n");
}

static void trace(Platform::String^ message) {
    trace(message->Data());
}

static void syslog(Platform::String^ message) {
    if (client == nullptr) {
        auto loghost = ref new HostName("172.16.8.1");
        
        client = ref new DatagramSocket();

        create_task(client->ConnectAsync(loghost, "18030")).then([message](task<void> conn) {
            conn.get();
            udpout = ref new DataWriter(client->OutputStream);
            syslog(message);
        });
    } else if (udpout != nullptr) {
        Platform::String^ timestamp = update_nowstamp();

        udpout->WriteByte('[');
        udpout->WriteString(timestamp);
        udpout->WriteString(L"] ");
        udpout->WriteString(message);
        create_task(udpout->StoreAsync()).then([timestamp, message](unsigned int size) {
            OutputDebugString(L"[");
            OutputDebugString(timestamp->Data());
            OutputDebugString(L"] ");
            OutputDebugString(message->Data());
            OutputDebugString(L"\n");
        });
    }
}

void rsyslog(Platform::String^ message) {
    syslog(message);
}

void rsyslog(const wchar_t *fmt, ...) {
    static const int DEFAULT_POOL_SIZE = 2048;
    static wchar_t pool[DEFAULT_POOL_SIZE];

    va_list argl;
    va_start(argl, fmt);
    vswprintf(pool, DEFAULT_POOL_SIZE, fmt, argl);
    va_end(argl);

    rsyslog(ref new Platform::String(pool));
}
