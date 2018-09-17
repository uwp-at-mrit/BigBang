#include <ppltasks.h>

#include "dirotation.hpp"
#include "syslog.hpp"
#include "string.hpp"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Storage;
using namespace Windows::Foundation;
using namespace Windows::Globalization;

/*************************************************************************************************/
IRotativeDirectory::IRotativeDirectory(Platform::String^ dirname, Rotation period, unsigned int count)
	: next(ref new Calendar()), period(period), count(max(count, 1)) {
	StorageFolder^ appdata = ApplicationData::Current->LocalFolder;
	Platform::String^ folder = ((dirname == nullptr) ? appdata->Path : appdata->Path + "\\" + dirname);
	
	create_task(StorageFolder::GetFolderFromPathAsync(folder)).then([=](task<StorageFolder^> building) {
		try {
			this->root = building.get();
			this->detect_time();
			this->on_folder_ready(this->root->Path, false);
		} catch (Platform::Exception^ e) {
			switch (e->HResult) {
			case HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND): {
				this->log_message(Log::Debug, make_wstring(L"Folder %s is not found, make it.", folder->Data()));
				create_task(appdata->CreateFolderAsync(dirname)).then([=](task<StorageFolder^> creating) {
					try {
						this->root = creating.get();
						this->detect_time();
						this->on_folder_ready(this->root->Path, true);
					} catch (Platform::Exception^ e) {
						this->on_exception(e);
					}
				});
			}; break;
			default: {
				this->on_exception(e);
			}
			}
		}
	});
}

IRotativeDirectory::~IRotativeDirectory() {
}

void IRotativeDirectory::detect_time() {
	static Calendar^ current = ref new Calendar();
	
	current->SetDateTime(this->root->DateCreated);
	current->Minute = 0;
	current->Second = 0;
	current->Nanosecond = 0;

	switch (this->period) {
	case Rotation::Hourly: {

		long long dt = current->GetDateTime().UniversalTime;
		//current->AddDays(this->count);
	} break;
	case Rotation::Daily: {
		//current->AddDays(this->count);
	} break;
	}
}

void IRotativeDirectory::on_exception(Platform::Exception^ e) {
	this->log_message(Log::Critical, e->Message);
	throw e;
}

void IRotativeDirectory::log_message(Log level, Platform::String^ message) {
	syslog(level, message);
}
