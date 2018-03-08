#pragma once

#include <queue>
#include <mutex>

#include "syslog/logging.hpp"

namespace WarGrey::SCADA {
	private class RacketReceiver : public WarGrey::SCADA::ISyslogReceiver {
	public:
		RacketReceiver(Platform::String^ server, unsigned short service,
			WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug,
			Platform::String^ topic = "");

	protected:
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data, Platform::String^ topic) override;

	protected:
		~RacketReceiver() noexcept {}

	private:
		std::mutex section;
		std::queue<Platform::String^> timestamps;
		std::queue<Platform::String^> levels;
		std::queue<Platform::String^> messages;

		Windows::Networking::Sockets::DatagramSocket^ client = nullptr;
		Windows::Storage::Streams::IDataWriter^ udpout = nullptr;
	};
}
