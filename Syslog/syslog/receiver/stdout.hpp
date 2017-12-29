#pragma once

#include "syslog/logging.hpp"

namespace WarGrey::SCADA {
	private class StdoutReceiver : public WarGrey::SCADA::ISyslogReceiver {
	public:
		StdoutReceiver(WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug, Platform::String^ topic = "")
			: ISyslogReceiver(level, topic) {};

	protected:
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::ISyslogData* data, Platform::String^ topic) override;
	};
}
