#pragma once

#include <queue>

#include "syslog/logging.hpp"

namespace WarGrey::SCADA {
	private class RSyslogReceiver : public WarGrey::SCADA::ISyslogReceiver {
	public:
		RSyslogReceiver(Platform::String^ server, unsigned short service,
			WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug,
			Platform::String^ topic = "");

	protected:
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::ISyslogData* data, Platform::String^ topic) override;

	private:
		std::queue<Platform::String^> timestamps;
		std::queue<Platform::String^> levels;
		std::queue<Platform::String^> messages;

		Windows::Networking::Sockets::DatagramSocket^ client = nullptr;
		Windows::Storage::Streams::IDataWriter^ udpout = nullptr;
	};
}
