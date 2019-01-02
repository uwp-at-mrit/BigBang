#include "backtask.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel::Background;

/**************************************************************************************/
BackgroundTaskBuilder^ WarGrey::SCADA::make_socket_task_builder(Platform::String^ name, Platform::String^ classname) {
	auto builder = ref new BackgroundTaskBuilder();
	auto trigger = ref new SocketActivityTrigger();
	auto condition = ref new SystemCondition(SystemConditionType::InternetAvailable);

	builder->Name = name;
	builder->IsNetworkRequested = true;

	if (classname != nullptr) {
		builder->TaskEntryPoint = classname;
	}
	
	builder->SetTrigger(trigger);
	builder->AddCondition(condition);

	return builder;
}
