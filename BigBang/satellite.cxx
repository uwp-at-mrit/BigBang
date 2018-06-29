#include "satellite.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

/** TODO
 * Why it complains "<Dispose> is not a member of `Satellite`"
 *  and "`IDisplay` does not have a user-defined copy constructor"
 * if delegating the construction to the second one;
 */
SatelliteDisplay::SatelliteDisplay(float width, float height, ISatellite* entity, Log level, Platform::String^ topic) {
	this->construct(width, height, entity, make_logger(level, ((topic == nullptr) ? entity->name() : topic)));
}

SatelliteDisplay::SatelliteDisplay(float width, float height, ISatellite* entity, Syslog* logger) {
	this->construct(width, height, entity, logger);
}

void SatelliteDisplay::construct(float width, float height, ISatellite* entity, Syslog* logger) {
	this->display = ref new UniverseDisplay(logger, entity);	
	this->display->width = width;
	this->display->height = height;

	this->Content = this->display->canvas;
	this->Placement = FlyoutPlacementMode::Full;

	this->Opening += ref new EventHandler<Platform::Object^>(this, &SatelliteDisplay::on_opening);
	this->Opened += ref new EventHandler<Platform::Object^>(this, &SatelliteDisplay::on_opened);
	this->Closing += ref new TypedEventHandler<FlyoutBase^, FlyoutBaseClosingEventArgs^>(this, &SatelliteDisplay::on_closing);
	this->Closed += ref new EventHandler<Platform::Object^>(this, &SatelliteDisplay::on_closed);
}

ISatellite* SatelliteDisplay::get_satellite() {
	return static_cast<ISatellite*>(this->display->current_planet);
}

Syslog* SatelliteDisplay::get_logger() {
	return this->display->get_logger();
}

void SatelliteDisplay::show(IPlanet* master) {
	this->ShowAt(master->info->master->canvas);
}

void SatelliteDisplay::on_opening(Platform::Object^ target, Platform::Object^ args) {
	this->get_satellite()->on_satellite_showing();
}

void SatelliteDisplay::on_opened(Platform::Object^ target, Platform::Object^ args) {
	this->get_satellite()->on_satellite_shown();
}

void SatelliteDisplay::on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
	args->Cancel = !(this->get_satellite()->on_satellite_hiding());
}

void SatelliteDisplay::on_closed(Platform::Object^ target, Platform::Object^ args) {
	this->get_satellite()->on_satellite_hiden();
}
