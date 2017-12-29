#include "syslog/receiver/rsyslog.hpp"

#include <ppltasks.h>

#include "syslog.hpp"
#include "string.hpp"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static void send_to(IDataWriter^ udpout, Platform::String^ ts, Platform::String^ level, Platform::String^ msg) {
	udpout->WriteByte('[');
	udpout->WriteString(ts);
	udpout->WriteString(L"] [");
	udpout->WriteString(level);
	udpout->WriteString(L"] ");
	udpout->WriteString(msg);
	udpout->StoreAsync();
}

RSyslogReceiver::RSyslogReceiver(Platform::String^ server, unsigned short service, Log level, Platform::String^ topic)
	: ISyslogReceiver(level, topic) {
	auto logserver = ref new HostName(server);

	this->client = client = ref new DatagramSocket();
	create_task(this->client->ConnectAsync(logserver, service.ToString())).then([this](task<void> conn) {
		conn.get();
		udpout = ref new DataWriter(client->OutputStream);

		do {
			auto ts = this->timestamps.front();
			auto lvl = this->levels.front();
			auto msg = this->messages.front();
			send_to(udpout, ts, lvl, msg);
			this->timestamps.pop();
			this->levels.pop();
			this->messages.pop();
		} while (!this->timestamps.empty());
	});
}

void RSyslogReceiver::on_log_message(Log level, Platform::String^ message, ISyslogData* data, Platform::String^ topic) {
	Platform::String^ timestamp = update_nowstamp();

	if (udpout == nullptr) {
		this->timestamps.push(timestamp);
		this->levels.push(level.ToString());
		this->messages.push(message);
	} else {
		send_to(udpout, timestamp, level.ToString(), message);
	}
}
