#include <shared_mutex>
#include <ppltasks.h>

#include "metrics.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Storage;

static SQLite3* sqlite3;
static std::shared_mutex section;

static void initialize() {
	CreationCollisionOption cco = CreationCollisionOption::OpenIfExists;

	enter_logbook_critical_section();
	create_task(ApplicationData::Current->LocalFolder->CreateFileAsync("log.ams", cco)).then([=](StorageFile^ file) {
		sqlite3 = new SQLite3(file->Path->Data());
		leave_logbook_critical_section();
	});
}

/*************************************************************************************************/
void WarGrey::SCADA::enter_logbook_critical_section() {
	section.lock();
}

void WarGrey::SCADA::leave_logbook_critical_section() {
	section.unlock();
}

void WarGrey::SCADA::enter_logbook_shared_section() {
	section.lock_shared();
}

void WarGrey::SCADA::leave_logbook_shared_section() {
	section.unlock_shared();
}

/*************************************************************************************************/
SQLite3* system_logbook() {
	SQLite3* logbook = nullptr;

	if (sqlite3 == nullptr) {
		initialize();
	}

	enter_logbook_shared_section();
	logbook = sqlite3;
	leave_logbook_shared_section();

	return logbook;
}
