#include <ppltasks.h>
#include <queue>

#include "rsyslog.hpp"
#include "time.hpp"

using namespace Concurrency;
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
    static std::queue<Platform::String^> timestamps;
    static std::queue<Platform::String^> messages;

    static DatagramSocket^ client = nullptr;
    static IDataWriter^ udpout = nullptr;

    Platform::String^ timestamp = update_nowstamp();

    if (client == nullptr) {
        auto loghost = ref new HostName("172.16.8.1");
        
        client = ref new DatagramSocket();
        timestamps.push(timestamp);
        messages.push(message);

        create_task(client->ConnectAsync(loghost, "18030")).then([message](task<void> conn) {
            conn.get();
            udpout = ref new DataWriter(client->OutputStream);
            
            do {
                auto ts = timestamps.front();
                auto msg = messages.front();
                send_to(udpout, ts, msg);
                timestamps.pop();
                messages.pop();
            } while (!timestamps.empty());
        });
    } else if (udpout == nullptr) {
        timestamps.push(timestamp);
        messages.push(message);
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
	VSWPRINT(pool, 2048, fmt);

    rsyslog(ref new Platform::String(pool));
}
