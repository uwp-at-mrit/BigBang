#pragma once

#include "syslog/logging.hpp"

namespace WarGrey::SCADA {
	private class VisualStudioReceiver : public WarGrey::SCADA::ISyslogReceiver {
	public:
		VisualStudioReceiver(WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug, Platform::String^ topic = "")
			: ISyslogReceiver(level, topic) {};

	protected:
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data, Platform::String^ topic) override;
	};
}
