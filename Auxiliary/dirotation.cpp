#include <map>

#include "dirotation.hpp"
#include "syslog.hpp"
#include "string.hpp"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;
using namespace Windows::Storage;

private ref class RotationListener sealed {
internal:
	RotationListener(IRotativeDirectory* master) : master(master) {}

public:
	void on_rotation(Platform::Object^ whocares, Platform::Object^ useless) {
		master->rotate_now();
	}

private:
	IRotativeDirectory* master;
};

/*************************************************************************************************/
IRotativeDirectory::IRotativeDirectory(Syslog* logger, Platform::String^ dirname
	, Platform::String^ prefix, Platform::String^ suffix, RotationPeriod period, unsigned int count)
	: logger(logger), period(period), period_count(max(count, 1)), file_prefix(prefix), file_suffix(suffix) {
	StorageFolder^ appdata = ApplicationData::Current->LocalFolder;
	Platform::String^ folder = ((dirname == nullptr) ? appdata->Path : appdata->Path + "\\" + dirname);
	RotationListener^ listener = ref new RotationListener(this);
	cancellation_token watcher = this->destructing_watcher.get_token();

	this->timer = ref new DispatcherTimer();
	this->timer->Tick += ref new EventHandler<Platform::Object^>(listener, &RotationListener::on_rotation);
	this->listener = listener;

	create_task(StorageFolder::GetFolderFromPathAsync(folder), watcher).then([=](task<StorageFolder^> building) {
		try {
			this->root = building.get();
			this->on_folder_ready(this->root->Path, false);
			this->mission_start();
		} catch (Platform::Exception^ e) {
			switch (e->HResult) {
			case HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND): {
				this->log_message(Log::Debug, make_wstring(L"Folder %s is not found, make it.", folder->Data()));
				create_task(appdata->CreateFolderAsync(dirname), watcher).then([=](task<StorageFolder^> creating) {
					try {
						this->root = creating.get();
						this->on_folder_ready(this->root->Path, true);
						this->mission_start();
					} catch (Platform::Exception^ e) {
						this->on_exception(e);
					} catch (task_canceled&) {}
				});
			}; break;
			default: {
				this->on_exception(e);
			}
			}
		} catch (task_canceled&) {}
	});
}

IRotativeDirectory::~IRotativeDirectory() {
	this->timer->Stop();
	this->destructing_watcher.cancel();
}

void IRotativeDirectory::rotate_now() {
	// NOTE: The timer is not precise in nature, sometimes it may be triggered early.
	long long next_timepoint = current_ceiling_seconds(this->span);
	Platform::String^ old_file = this->current_file;

	this->current_file = this->resolve_current_file();

	if (this->current_file->Equals(old_file)) {
		long long ms = next_timepoint * 1000LL - current_milliseconds();
		
		sleep(ms);
		this->logger->log_message(Log::Notice, L"Sleepped %lldms since the file rotation event is triggered early", ms);

		this->rotate_now();
	} else {
		this->timer->Interval = this->resolve_interval();
		this->on_file_rotated(old_file, this->current_file);
	}
}

void IRotativeDirectory::on_exception(Platform::Exception^ e) {
	this->log_message(Log::Critical, e->Message);
	throw e;
}

void IRotativeDirectory::log_message(Log level, Platform::String^ message) {
	this->logger->log_message(level, message);
}

Platform::String^ IRotativeDirectory::resolve_current_file() {
	long long timepoint = current_floor_seconds(this->span);
	Platform::String^ file_name = this->file_prefix
		+ "-" + make_datestamp_utc(timepoint, true)
		+ "T" + make_daytimestamp_utc(timepoint, true)
		+ this->file_suffix;

	return file_name;
}

TimeSpan IRotativeDirectory::resolve_interval() {
	TimeSpan timepoint = make_timespan_from_seconds(current_ceiling_seconds(this->span));

	timepoint.Duration -= current_100nanoseconds();

	return timepoint;
}

void IRotativeDirectory::mission_start() {
	switch (this->period) {
	case RotationPeriod::Daily: {
		this->span = day_span_s;
	} break;
	case RotationPeriod::Hourly: {
		this->span = hour_span_s;
	} break;
	case RotationPeriod::Minutely: {
		this->span = minute_span_s;
	} break;
	default: {
		this->span = 1LL;
	}
	}

	this->span *= this->period_count;
	this->current_file = this->resolve_current_file();
	
	{ // resolve the next timepoint
		this->timer->Interval = this->resolve_interval();
		this->timer->Start();
	}
}
