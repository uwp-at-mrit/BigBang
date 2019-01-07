#include "timemachine.hpp"

#include "navigator/listview.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Core;
using namespace Windows::UI::Input;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

static void configure_flyout(Flyout^ orbit, ITimeMachine* self) {
	Style^ style = ref new Style(FlyoutPresenter::typeid); // WARNING: the style can only be modified before it is applied
	Thickness border, padding;
	double wspread, hspread;
	float width, height;

	self->fill_timemachine_extent(&width, &height);
	self->fill_timemachine_border(border);
	self->fill_timemachine_padding(padding);

	wspread = (border.Left + border.Right + padding.Left + padding.Right);
	hspread = (border.Top + border.Bottom + padding.Top + padding.Bottom);

	style->Setters->Append(ref new Setter(FlyoutPresenter::BorderThicknessProperty, border));
	style->Setters->Append(ref new Setter(FlyoutPresenter::PaddingProperty, padding));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxWidthProperty, double(width) + wspread));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxHeightProperty, double(height) + hspread));

	orbit->FlyoutPresenterStyle = style; // apply the style
}

/*************************************************************************************************/
private ref class TimeMachineUniverseDisplay sealed : public UniverseDisplay {
internal:
	TimeMachineUniverseDisplay(Syslog* logger, ITimeMachine* entity)
		: UniverseDisplay(logger, nullptr, new ListViewNavigator(), nullptr)
		, machine(entity), closed(true) {
	
		this->use_global_mask_setting(false);

		this->_void = ref new SplitView();
		this->_void->Content = this->canvas;
		this->_void->Pane = this->navigator->user_interface();

		this->_void->OpenPaneLength = 256.0;
		this->_void->DisplayMode = SplitViewDisplayMode::Overlay;
		
		this->_void->PointerReleased += ref new PointerEventHandler(this, &TimeMachineUniverseDisplay::on_pointer_released);
	}

internal:
	void push_planet(IPlanet* planet) {
		UniverseDisplay::push_planet(planet);
	}

public:
	SplitView^ flyout_content() {
		return this->_void;
	}

public:
	void on_opening(Platform::Object^ target, Platform::Object^ args) {
		this->darkness = this->global_mask_alpha;
		this->machine->on_timemachine_showing();
	}

	void on_opened(Platform::Object^ target, Platform::Object^ args) {
		this->closed = false;
		this->global_mask_alpha = 0.8;
		this->_void->IsPaneOpen = false;
		this->machine->on_timemachine_shown();
	}

	void on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
		args->Cancel = !(this->machine->can_timemachine_hiding());
	}

	void on_closed(Platform::Object^ target, Platform::Object^ args) {
		this->closed = true;
		this->machine->on_timemachine_hiden();
		this->global_mask_alpha = this->darkness;
	}

public:
	bool shown() override {
		return !(this->closed);
	}

protected:
	void construct() override {
		this->machine->construct();
	}

private:
	void on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
		auto pt = args->GetCurrentPoint(this->_void);
		
		if (pt->Properties->PointerUpdateKind == PointerUpdateKind::LeftButtonReleased) {
			this->_void->IsPaneOpen = !this->_void->IsPaneOpen;
			args->Handled = true;
		}
	}

private:
	SplitView^ _void;
	ITimeMachine* machine; // its lifetime is managed by client applications
	double darkness;
	bool closed;
};

/*************************************************************************************************/
ITimeMachine::ITimeMachine(Syslog* logger) : ready(false) {
	TimeMachineUniverseDisplay^ _universe = ref new TimeMachineUniverseDisplay(logger, this);

	this->machine = ref new Flyout();
	this->machine->Content = _universe->flyout_content();
	this->machine->Placement = FlyoutPlacementMode::Full;
	
	this->machine->Opening += ref new EventHandler<Platform::Object^>(_universe, &TimeMachineUniverseDisplay::on_opening);
	this->machine->Opened += ref new EventHandler<Platform::Object^>(_universe, &TimeMachineUniverseDisplay::on_opened);
	this->machine->Closing += ref new TypedEventHandler<FlyoutBase^, FlyoutBaseClosingEventArgs^>(_universe, &TimeMachineUniverseDisplay::on_closing);
	this->machine->Closed += ref new EventHandler<Platform::Object^>(_universe, &TimeMachineUniverseDisplay::on_closed);
	
	this->universe = _universe;
	FlyoutBase::SetAttachedFlyout(this->universe->canvas, this->machine);
}

void ITimeMachine::push_planet(IPlanet* planet) {
	auto tmud = dynamic_cast<TimeMachineUniverseDisplay^>(this->universe);

	if (tmud != nullptr) {
		tmud->push_planet(planet);
	}
}

void ITimeMachine::fill_timemachine_border(Thickness& border) {
	double thickness = 1.0;
	
	border.Top = thickness;
	border.Right = thickness;
	border.Bottom = thickness;
	border.Left = thickness;
}

void ITimeMachine::fill_timemachine_padding(Thickness& padding) {
	double space = 0.0;

	padding.Top = space;
	padding.Right = space;
	padding.Bottom = space;
	padding.Left = space;
}

Syslog* ITimeMachine::get_logger() {
	return this->universe->get_logger();
}

void ITimeMachine::show() {
	try {
		FrameworkElement^ frame = dynamic_cast<FrameworkElement^>(Window::Current->Content);
		
		if (this->machine->FlyoutPresenterStyle == nullptr) {
			configure_flyout(this->machine, this);
		}

		this->machine->ShowAt(frame);
	} catch (Platform::Exception^ e) {
		this->universe->get_logger()->log_message(Log::Critical, e->Message);
	}
}

void ITimeMachine::hide() {
	this->machine->Hide();
}

void ITimeMachine::notify_surface_ready() {
	this->ready = true;
	this->on_surface_ready();
}

bool ITimeMachine::surface_ready() {
	// Surpass the cross-thread accessing.
	return this->ready;
}
