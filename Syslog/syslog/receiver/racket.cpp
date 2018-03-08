#include <ppltasks.h>

#include "syslog/receiver/racket.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static void send_to(IDataWriter^ udpout, Platform::String^ ts, Platform::String^ level, Platform::String^ msg) {
	udpout->WriteByte('<');
	udpout->WriteString(ts);
	udpout->WriteString(L"> [");
	udpout->WriteString(level);
	udpout->WriteString(L"] ");
	udpout->WriteString(msg);
	udpout->StoreAsync();
}

RacketReceiver::RacketReceiver(Platform::String^ server, unsigned short service, Log level, Platform::String^ topic)
	: ISyslogReceiver(level, topic) {
	auto logserver = ref new HostName(server);

	this->client = client = ref new DatagramSocket();
	create_task(this->client->ConnectAsync(logserver, service.ToString())).then([this](task<void> conn) {
		conn.get();
		udpout = ref new DataWriter(client->OutputStream);

		this->section.lock();
		while (!this->timestamps.empty()) {
			auto ts = this->timestamps.front();
			auto lvl = this->levels.front();
			auto msg = this->messages.front();
			send_to(udpout, ts, lvl, msg);
			this->timestamps.pop();
			this->levels.pop();
			this->messages.pop();
		};
		this->section.unlock();
	});
}

void RacketReceiver::on_log_message(Log level, Platform::String^ message, SyslogMetainfo& data, Platform::String^ topic) {
	Platform::String^ timestamp = data.timestamp;

	if (udpout == nullptr) {
		this->section.lock();
		this->levels.push(level.ToString());
		this->messages.push(message);
		this->timestamps.push(timestamp);
		this->section.unlock();
	} else {
		send_to(udpout, timestamp, level.ToString(), message);
	}
}
