#include <ppltasks.h>
#include <collection.h>

#include "rsyslog.hpp"
#include "time.hpp"

using namespace Concurrency;
using namespace Platform::Collections;

using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static void trace(const wchar_t* message) {
    OutputDebugString(message);
    OutputDebugString(L"\n");
}

static void trace(Platform::String^ message) {
    trace(message->Data());
}

static void send_to(IDataWriter^ udpout, Platform::String^ ts, Platform::String^ msg) {
    udpout->WriteByte('[');
    udpout->WriteString(ts);
    udpout->WriteString(L"] ");
    udpout->WriteString(msg);
    udpout->StoreAsync();
}

static void syslog(Platform::String^ message) {
    static Vector<Platform::String^>^ timestamps = ref new Vector<Platform::String^>();
    static Vector<Platform::String^>^ messages = ref new Vector<Platform::String^>();

    static DatagramSocket^ client = nullptr;
    static IDataWriter^ udpout = nullptr;

    Platform::String^ timestamp = update_nowstamp();

    if (client == nullptr) {
        auto loghost = ref new HostName("172.16.8.1");
        
        client = ref new DatagramSocket();
        timestamps->Append(timestamp);
        messages->Append(message);

        create_task(client->ConnectAsync(loghost, "18030")).then([message](task<void> conn) {
            conn.get();
            udpout = ref new DataWriter(client->OutputStream);
            
            do {
                auto ts = timestamps->GetAt(0);
                auto msg = messages->GetAt(0);
                send_to(udpout, ts, msg);
                timestamps->RemoveAt(0);
                messages->RemoveAt(0);
            } while (timestamps->Size > 0);
        });
    } else if (udpout == nullptr) {
        timestamps->Append(timestamp);
        messages->Append(message);
    } else {
        send_to(udpout, timestamp, message);
    }

    OutputDebugString(L"[");
    OutputDebugString(timestamp->Data());
    OutputDebugString(L"] ");
    OutputDebugString(message->Data());
    OutputDebugString(L"\n");
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
