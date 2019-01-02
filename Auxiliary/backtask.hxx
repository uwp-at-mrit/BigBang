#pragma once

namespace WarGrey::SCADA {
	Windows::ApplicationModel::Background::BackgroundTaskBuilder^ make_socket_task_builder(Platform::String^ name,
		Platform::String^ classname = nullptr);
}
