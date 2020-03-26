#include <ppltasks.h>

#include "display.hxx"

#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/flonum.hpp"

#include "system.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::GYDM;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;
using namespace Windows::ApplicationModel;

using namespace Microsoft::Graphics::Canvas;

#define PLANET_INFO(planet) (static_cast<PlanetInfo*>(planet->info))

/*************************************************************************************************/
IDisplay::IDisplay(Syslog* logger) : logger((logger == nullptr) ? make_silent_logger("IDisplay") : logger) {
	this->logger->reference();
}

IDisplay::~IDisplay() {
	this->logger->destroy();
}

bool IDisplay::shown() {
	// TODO: The VisualTree should get involved
	return true;
}

float IDisplay::actual_width::get() {
	return float(this->canvas->ActualWidth);
}

float IDisplay::actual_height::get() {
	return float(this->canvas->ActualHeight);
}

void IDisplay::max_width::set(float v) {
	this->canvas->MaxWidth = double(v);
}

float IDisplay::max_width::get() {
	return float(this->canvas->MaxWidth);
}

void IDisplay::max_height::set(float v) {
	this->canvas->MaxHeight = double(v);
}

float IDisplay::max_height::get() {
	return float(this->canvas->MaxHeight);
}

void IDisplay::min_width::set(float v) {
	this->canvas->MinWidth = double(v);
}

float IDisplay::min_width::get() {
	return float(this->canvas->MinWidth);
}

void IDisplay::min_height::set(float v) {
	this->canvas->MinHeight = double(v);
}

float IDisplay::min_height::get() {
	return float(this->canvas->MinHeight);
}

void IDisplay::width::set(float v) {
	this->canvas->Width = double(v);
}

float IDisplay::width::get() {
	return float(this->canvas->Width);
}

void IDisplay::height::set(float v) {
	this->canvas->Height = double(v);
}

float IDisplay::height::get() {
	return float(this->canvas->Height);
}

Point IDisplay::global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff, float yoff) {
	return Point(global_x + xoff, global_y + yoff);
}

Point IDisplay::local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff, float yoff) {
	return Point(local_x + xoff, local_y + yoff);
}

float IDisplay::planet_actual_width(IPlanet* p) {
	return this->actual_width;
}

float IDisplay::planet_actual_height(IPlanet* p) {
	return this->actual_height;
}

void IDisplay::enter_critical_section() {
	this->section.lock();
}

void IDisplay::leave_critical_section() {
	this->section.unlock();
}

Syslog* IDisplay::get_logger() {
	return this->logger;
}

void IDisplay::save(Platform::String^ path, float dpi) {
	CanvasRenderTarget^ snapshot = this->take_snapshot(dpi);

	if (path_only(path) == nullptr) {
		CreationCollisionOption oie = CreationCollisionOption::OpenIfExists;
		CreationCollisionOption re = CreationCollisionOption::ReplaceExisting;
		Platform::String^ root = Package::Current->DisplayName;

		/** WARNING: Stupid Windows 10
		 * Saving through `IRandomAccessStream` is the only working way,
		 * and the `CanvasBitmapFileFormat::Auto` option is lost.
		 */

		create_task(KnownFolders::PicturesLibrary->CreateFolderAsync(root, oie)).then([=](task<StorageFolder^> getting) {
			return create_task(getting.get()->CreateFileAsync(path, re)).then([=](task<StorageFile^> creating) {
				return create_task(creating.get()->OpenAsync(FileAccessMode::ReadWrite)).then([=](task<IRandomAccessStream^> opening) {
					return create_task(snapshot->SaveAsync(opening.get(), CanvasBitmapFileFormat::Png, 1.0F));
				});
			});
		}).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice, L"universe[%s] has been saved to [My Picture]\\%s\\%s",
					this->get_logger()->get_name()->Data(), root->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic, L"failed to save universe[%s] to [My Pictures]\\%s\\%s: %s",
					this->get_logger()->get_name()->Data(), root->Data(), path->Data(), e->Message->Data());
			}
		});
	} else {
		create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice, L"universe[%s] has been saved to %s",
					this->get_logger()->get_name()->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic, L"failed to save universe[%s] to %s: %s",
					this->get_logger()->get_name()->Data(), path->Data(), e->Message->Data());
			}
		});
	}
}
