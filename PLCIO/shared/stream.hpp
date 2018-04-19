#pragma once

namespace WarGrey::SCADA {
	private class ISocketAcceptable abstract {
	public:
		virtual void on_socket(Windows::Networking::Sockets::StreamSocket^ socket) = 0;
	};

	Platform::String^ socket_description(Windows::Networking::Sockets::StreamSocket^ socket);

	Windows::Storage::Streams::DataReader^ make_socket_reader(Windows::Networking::Sockets::StreamSocket^ socket);
	Windows::Storage::Streams::DataWriter^ make_socket_writer(Windows::Networking::Sockets::StreamSocket^ socket);

	private class StreamListener {
	public:
		virtual ~StreamListener() noexcept;

		StreamListener();

	public:
		void listen(ISocketAcceptable* master, Platform::String^ service);

	private:
		Windows::Networking::Sockets::StreamSocketListener^ entity;
	};
}
