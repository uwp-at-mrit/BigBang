#include "satellite.hpp"
#include "navigator/null.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Core;
using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

static void configure_flyout(Flyout^ orbit, ISatellite* self) {
	Style^ style = ref new Style(FlyoutPresenter::typeid); // WARNING: the style can only be modified before it is applied
	Thickness border, padding;
	double wspread, hspread;
	float width, height;

	self->fill_satellite_extent(&width, &height);
	self->fill_satellite_border(border);
	self->fill_satellite_padding(padding);

	wspread = (border.Left + border.Right + padding.Left + padding.Right);
	hspread = (border.Top + border.Bottom + padding.Top + padding.Bottom);

	style->Setters->Append(ref new Setter(FlyoutPresenter::BorderThicknessProperty, border));
	style->Setters->Append(ref new Setter(FlyoutPresenter::PaddingProperty, padding));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxWidthProperty, double(width) + wspread));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxHeightProperty, double(height) + hspread));

	orbit->FlyoutPresenterStyle = style; // apply the style
}

/*************************************************************************************************/
private ref class SatelliteDisplay sealed : public UniverseDisplay {
internal:
	SatelliteDisplay(Syslog* logger, ISatellite* entity)
		: UniverseDisplay(logger, nullptr, entity, new NullNavigator())
		, satellite(entity), closed(true) {}

public:
	void on_opening(Platform::Object^ target, Platform::Object^ args) {
		this->satellite->on_satellite_showing();
	}

	void on_opened(Platform::Object^ target, Platform::Object^ args) {
		this->closed = false;
		this->satellite->on_satellite_shown();
	}

	void on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
		args->Cancel = !(this->satellite->can_satellite_hiding());
	}

	void on_closed(Platform::Object^ target, Platform::Object^ args) {
		this->closed = true;
		this->satellite->on_satellite_hiden();
	}

public:
	bool shown() override {
		return !(this->closed);
	}

protected:
	void collapse() override {
		/** NOTE
		 * By design, `ISatellite` is the only interface that client applications would use,
		 *  and therefore they should take responsibility to manage the lifecycle of the instance.
		 *
		 * Here the method `collapse` is overriden to avoid deleting the instance twice.
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

void ISatellite::fill_satellite_border(Thickness& border) {
	double thickness = 1.0;
	
	border.Top = thickness;
	border.Right = thickness;
	border.Bottom = thickness;
	border.Left = thickness;
}

void ISatellite::fill_satellite_padding(Thickness& padding) {
	double space = 0.0;

	padding.Top = space;
	padding.Right = space;
	padding.Bottom = space;
	padding.Left = space;
}

void ISatellite::show() {
	try {
		FrameworkElement^ frame = dynamic_cast<FrameworkElement^>(Window::Current->Content);
		
		if (this->orbit->FlyoutPresenterStyle == nullptr) {
			configure_flyout(this->orbit, this);
		}

		this->orbit->ShowAt(frame);
	} catch (Platform::Exception^ e) {
		this->get_logger()->log_message(Log::Critical, e->Message);
	}
}

void ISatellite::hide() {
	this->orbit->Hide();
}

void ISatellite::notify_surface_ready() {
	this->ready = true;
	this->on_surface_ready();
}

bool ISatellite::surface_ready() {
	// Surpass the cross-thread accessing.
	return this->ready;
}
