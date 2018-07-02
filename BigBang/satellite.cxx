#include "satellite.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

void ISatellite::hide() {
	if (this->info != nullptr) {
		auto orbit = FlyoutBase::GetAttachedFlyout(this->info->master->canvas);

		if (orbit != nullptr) {
			orbit->Hide();
		}
	}
}

/*************************************************************************************************/

/** TODO
 * Why it complains "<Dispose> is not a member of `Satellite`"
 *  and "`IDisplay` does not have a user-defined copy constructor"
 * if delegating the construction to the second one;
 */
SatelliteOrbit::SatelliteOrbit(ISatellite* entity, Log level, Platform::String^ topic) {
	this->construct(entity, make_system_logger(level, ((topic == nullptr) ? entity->name() : topic)));
}

SatelliteOrbit::SatelliteOrbit(ISatellite* entity, Syslog* logger) {
	this->construct(entity, logger);
}

void SatelliteOrbit::construct(ISatellite* entity, Syslog* logger) {
	this->display = ref new UniverseDisplay(logger, entity);
	this->Content = this->display->canvas;
	this->Placement = FlyoutPlacementMode::Full;

	FlyoutBase::SetAttachedFlyout(this->display->canvas, this);

	this->Opening += ref new EventHandler<Platform::Object^>(this, &SatelliteOrbit::on_opening);
	this->Opened += ref new EventHandler<Platform::Object^>(this, &SatelliteOrbit::on_opened);
	this->Closing += ref new TypedEventHandler<FlyoutBase^, FlyoutBaseClosingEventArgs^>(this, &SatelliteOrbit::on_closing);
	this->Closed += ref new EventHandler<Platform::Object^>(this, &SatelliteOrbit::on_closed);
}

ISatellite* SatelliteOrbit::get_satellite() {
	return static_cast<ISatellite*>(this->display->current_planet);
}

Syslog* SatelliteOrbit::get_logger() {
	return this->display->get_logger();
}

void SatelliteOrbit::show(IPlanet* master) {
	this->ShowAt(master->info->master->canvas);
}

void SatelliteOrbit::on_opening(Platform::Object^ target, Platform::Object^ args) {
	if (this->FlyoutPresenterStyle == nullptr) {
		/** WARNING
		 * When the style is applied to the Flyout object, it cannot be modified.
		 */
		Style^ style = ref new Style(FlyoutPresenter::typeid);
		float width, height;
		
		this->get_satellite()->fill_satellite_extent(&width, &height);

		style->Setters->Append(ref new Setter(FrameworkElement::MaxWidthProperty, double(width)));
		style->Setters->Append(ref new Setter(FrameworkElement::MaxHeightProperty, double(height)));

		this->FlyoutPresenterStyle = style; // apply the style
	}

	this->get_satellite()->on_satellite_showing();
}

void SatelliteOrbit::on_opened(Platform::Object^ target, Platform::Object^ args) {
	this->get_satellite()->on_satellite_shown();
}

void SatelliteOrbit::on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
	args->Cancel = !(this->get_satellite()->can_satellite_hiding());
}

void SatelliteOrbit::on_closed(Platform::Object^ target, Platform::Object^ args) {
	this->get_satellite()->on_satellite_hiden();
}
