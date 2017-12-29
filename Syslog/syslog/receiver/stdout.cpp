#include <windows.h>

#include "syslog/receiver/stdout.hpp"

using namespace WarGrey::SCADA;

void StdoutReceiver::on_log_message(Log level, Platform::String^ message, ISyslogData* whocares, Platform::String^ topic) {
	auto actual_message = "[" + level.ToString() + "] " + message;

	OutputDebugString(actual_message->Data());
	OutputDebugString(L"\n");
}
