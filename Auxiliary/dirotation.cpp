#include <map>

#include "dirotation.hpp"

#include "syslog.hpp"
#include "string.hpp"
#include "path.hpp"

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
		master->do_rotating_with_this_bad_named_function_which_not_designed_for_client_applications();
	}

private:
	IRotativeDirectory* master;
};

/*************************************************************************************************/
IRotativeDirectory::IRotativeDirectory(Platform::String^ dirname
	, Platform::String^ prefix, Platform::String^ suffix, RotationPeriod period, unsigned int count)
	: period(period), period_count(max(count, 1)), file_prefix(prefix), file_suffix(suffix) {
	StorageFolder^ appdata = ApplicationData::Current->LocalFolder;
	Platform::String^ folder = ((dirname == nullptr) ? appdata->Path : appdata->Path + "\\" + dirname);
	RotationListener^ listener = ref new RotationListener(this);
	cancellation_token watcher = this->destructing_watcher.get_token();

	this->timer = ref new DispatcherTimer();
	this->timer->Tick += ref new EventHandler<Platform::Object^>(listener, &RotationListener::on_rotation);
	this->listener = listener;

	create_task(StorageFolder::GetFolderFromPathAsync(folder), watcher).then([=](task<StorageFolder^> getting) {
		try {
			this->root = getting.get();
			this->on_folder_ready(this->root, false);
			this->mission_start();
		} catch (Platform::Exception^ e) {
			switch (e->HResult) {
			case HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND): {
				create_task(appdata->CreateFolderAsync(dirname), watcher).then([=](task<StorageFolder^> creating) {
					try {
						this->root = creating.get();
						this->on_folder_ready(this->root, true);
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

bool IRotativeDirectory::root_ready() {
	return (this->root != nullptr);
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

	if (this->file_prefix == nullptr) {
		this->file_prefix = this->root->Name;
	}

	{ // create or reuse the current file
		CreationCollisionOption cco = CreationCollisionOption::OpenIfExists;
		cancellation_token watcher = this->destructing_watcher.get_token();
		long long now = current_seconds();
		Platform::String^ current_file = this->resolve_filename(now);

		create_task(this->root->CreateFileAsync(current_file, cco), watcher).then([=](task<StorageFile^> creating) {
			try {
				this->current_file = creating.get();
				this->on_file_reused(this->current_file, this->timepoint(now));

				this->timer->Interval = this->resolve_interval();
				this->timer->Start();
			} catch (Platform::Exception^ e) {
				this->on_exception(e);
			} catch (task_canceled&) {}
		});
	}
}

void IRotativeDirectory::do_rotating_with_this_bad_named_function_which_not_designed_for_client_applications() {
	// NOTE: The timer is not precise in nature, sometimes it may be triggered early.
	long long next_timepoint = current_ceiling_seconds(this->span);
	long long now = current_seconds();
	Platform::String^ current_file = this->resolve_filename(now);

	if (this->current_file->Name->Equals(current_file)) {
		long long ms = next_timepoint * 1000LL - current_milliseconds();
		
		sleep(ms);
		this->do_rotating_with_this_bad_named_function_which_not_designed_for_client_applications();
	} else {
		CreationCollisionOption cco = CreationCollisionOption::ReplaceExisting;
		cancellation_token watcher = this->destructing_watcher.get_token();

		create_task(this->root->CreateFileAsync(current_file, cco), watcher).then([=](task<StorageFile^> creating) {
			try {
				StorageFile^ prev_file = this->current_file;

				this->current_file = creating.get();
				this->on_file_rotated(prev_file, this->current_file, this->timepoint(now));

				this->timer->Interval = this->resolve_interval();
			} catch (Platform::Exception^ e) {
				this->on_exception(e);
			} catch (task_canceled&) {}
		});
	}
}

void IRotativeDirectory::on_exception(Platform::Exception^ exn) {
	throw exn;
}

Platform::String^ IRotativeDirectory::resolve_filename(long long time_s) {
	long long ts = this->timepoint(time_s);

	// Stupid Windows Machine, ":" cannot be included in filename
	return this->file_prefix
		+ "_" + this->period_count.ToString() + this->period.ToString()
		+ "-" + file_basename_from_second(ts)
		+ this->file_suffix;
}

Platform::String^ IRotativeDirectory::resolve_pathname(long long time_s) {
	Platform::String^ filename = this->resolve_filename(time_s);

	return ((this->root_ready()) ? (this->root->Path + "\\" + filename) : filename);
}

TimeSpan IRotativeDirectory::resolve_interval() {
	TimeSpan timepoint = make_timespan_from_seconds(current_ceiling_seconds(this->span));

	timepoint.Duration -= current_100nanoseconds();

	return timepoint;
}

void IRotativeDirectory::on_file_reused(StorageFile^ file, long long timepoint) {
	this->on_file_rotated(nullptr, file, timepoint);
}

StorageFolder^ IRotativeDirectory::rootdir() {
	return this->root;
}

long long IRotativeDirectory::timepoint(long long time_s) {
	return floor_seconds(time_s * 1000LL * 1000LL * 10LL, this->span);
}

long long IRotativeDirectory::span_seconds() {
	return this->span;
}
