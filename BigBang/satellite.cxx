#include "satellite.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

/*************************************************************************************************/
private ref class SatelliteDisplay sealed : public UniverseDisplay {
internal:
	SatelliteDisplay(Syslog* logger, ISatellite* entity)
		: UniverseDisplay(logger, nullptr, nullptr, entity)
		, satellite(entity), closed(true) {}

public:
	void on_opening(Platform::Object^ target, Platform::Object^ args) {
		this->satellite->on_showing();
	}

	void on_opened(Platform::Object^ target, Platform::Object^ args) {
		this->closed = false;
		this->satellite->on_shown();
	}

	void on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
		args->Cancel = !(this->satellite->can_hide());
	}

	void on_closed(Platform::Object^ target, Platform::Object^ args) {
		this->closed = true;
		this->satellite->on_hiden();
	}

public:
	bool shown() override {
		return !(this->closed);
	}

protected:
	void collapse() override {
		/** NOTE
		 * By design, this is triggered after `deleting` the `satellite` by client applications,
		 *  therefore, the `collapse` is overriden to avoid deleting the `satellite` twice.
		 */
	}

private:
	ISatellite* satellite; // its lifetime is managed by client applications
	bool closed;
};

/*************************************************************************************************/
ISatellite::ISatellite(Log level, Platform::String^ caption, unsigned int initial_mode)
	: ISatellite(make_system_logger(level, caption), caption, initial_mode) {}

ISatellite::ISatellite(Syslog* logger, Platform::String^ caption, unsigned int initial_mode) : Planet(caption, initial_mode), ready(false) {
	SatelliteDisplay^ surface = ref new SatelliteDisplay(logger, this);

	this->orbit = ref new Flyout();
	this->orbit->Content = surface->canvas;
	this->orbit->Placement = FlyoutPlacementMode::Full;
	
	this->orbit->Opening += ref new EventHandler<Platform::Object^>(surface, &SatelliteDisplay::on_opening);
	this->orbit->Opened += ref new EventHandler<Platform::Object^>(surface, &SatelliteDisplay::on_opened);
	this->orbit->Closing += ref new TypedEventHandler<FlyoutBase^, FlyoutBaseClosingEventArgs^>(surface, &SatelliteDisplay::on_closing);
	this->orbit->Closed += ref new EventHandler<Platform::Object^>(surface, &SatelliteDisplay::on_closed);

	FlyoutBase::SetAttachedFlyout(surface->canvas, this->orbit);
}

void ISatellite::notify_surface_ready() {
	this->ready = true;
	this->on_surface_ready();
}

bool ISatellite::surface_ready() {
	// Surpass the cross-thread accessing.
	return this->ready;
}

Flyout^ ISatellite::user_interface() {
	return this->orbit;
}
