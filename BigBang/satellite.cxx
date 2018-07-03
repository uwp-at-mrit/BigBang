#include "satellite.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

void ISatellite::fill_satellite_border(Thickness& border) {
	border.Top = 1.0;
	border.Right = 1.0;
	border.Bottom = 1.0;
	border.Left = 1.0;
}

void ISatellite::fill_satellite_padding(Thickness& padding) {
	double margin = 8.0;

	padding.Top = margin;
	padding.Right = margin;
	padding.Bottom = margin;
	padding.Left = margin;
}

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

SatelliteOrbit::~SatelliteOrbit() {
	/** TODO
 	 * Why deleting a `SatelliteOrbit` object does not destory its `UniverseDisplay` object automatically.
	 * Does this destructor really do what it is expected to do?
	 */

	delete this->display;
}

void SatelliteOrbit::construct(ISatellite* entity, Syslog* logger) {
	this->display = ref new UniverseDisplay(logger, entity);
	this->Content = this->display->canvas;
	this->Placement = FlyoutPlacementMode::Full;

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
		ISatellite* entity = this->get_satellite();
		Thickness border, padding;
		double wspread, hspread;
		float width, height;
		
		entity->fill_satellite_extent(&width, &height);
		entity->fill_satellite_border(border);
		entity->fill_satellite_padding(padding);

		wspread = (border.Left + border.Right + padding.Left + padding.Right);
		hspread = (border.Top + border.Bottom + padding.Top + padding.Bottom);

		style->Setters->Append(ref new Setter(FlyoutPresenter::BorderThicknessProperty, border));
		style->Setters->Append(ref new Setter(FlyoutPresenter::PaddingProperty, padding));
		style->Setters->Append(ref new Setter(FlyoutPresenter::MaxWidthProperty, double(width) + wspread));
		style->Setters->Append(ref new Setter(FlyoutPresenter::MaxHeightProperty, double(height) + hspread));
		
		this->FlyoutPresenterStyle = style; // apply the style

		FlyoutBase::SetAttachedFlyout(this->display->canvas, this);
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
