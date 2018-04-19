#include <ppltasks.h>
#include <map>

#include "shared/stream.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

// delegate only accepts C++/CX class
private ref class GhostListener sealed {
internal:
	GhostListener(ISocketAcceptable* master) : master(master) {}

public:
	void respond(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
		this->master->on_socket(e->Socket);
	}

private:
	ISocketAcceptable* master;
};

static std::map<int, GhostListener^> ghosts;

StreamListener::StreamListener() {
	this->entity = ref new StreamSocketListener();
	
	this->entity->Control->QualityOfService = SocketQualityOfService::LowLatency;
	this->entity->Control->KeepAlive = false;
	this->entity->Control->NoDelay = true;
}

StreamListener::~StreamListener() {
	auto uuid = this->entity->GetHashCode();
	auto self = ghosts.find(uuid);

	if (self != ghosts.end()) {
		ghosts.erase(self);
	}
}

void StreamListener::listen(ISocketAcceptable* master, Platform::String^ service) {
	auto delegate = ref new GhostListener(master);
	auto uuid = this->entity->GetHashCode();

	ghosts.insert(std::pair<int, GhostListener^>(uuid, delegate));

	create_task(this->entity->BindEndpointAsync(nullptr, service)).wait();

	this->entity->ConnectionReceived
		+= ref new TypedEventHandler<StreamSocketListener^, StreamSocketListenerConnectionReceivedEventArgs^>(
			delegate, &GhostListener::respond);
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::socket_remote_description(StreamSocket^ client) {
	return client->Information->RemoteHostName->RawName + ":" + client->Information->RemotePort;
}

Platform::String^ WarGrey::SCADA::socket_local_description(StreamSocket^ client) {
	return client->Information->LocalAddress->RawName + ":" + client->Information->LocalPort;
}

DataReader^ WarGrey::SCADA::make_socket_reader(StreamSocket^ socket) {
	DataReader^ sktin = ref new DataReader(socket->InputStream);
	
	sktin->UnicodeEncoding = UnicodeEncoding::Utf8;
	sktin->ByteOrder = ByteOrder::BigEndian;

	return sktin;
}

DataWriter^ WarGrey::SCADA::make_socket_writer(StreamSocket^ socket) {
	DataWriter^ sktout = ref new DataWriter(socket->OutputStream);

	sktout->UnicodeEncoding = UnicodeEncoding::Utf8;
	sktout->ByteOrder = ByteOrder::BigEndian;

	return sktout;
}
