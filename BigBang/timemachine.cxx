#include "timemachine.hpp"

#include "navigator/listview.hpp"

#include "string.hpp"
#include "timer.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Storage;

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
	TimeMachineUniverseDisplay(Syslog* logger, ITimeMachine* entity, int frame_rate)
		: UniverseDisplay(logger, nullptr, new ListViewNavigator(), nullptr)
		, machine(entity), closed(true) {
	
		this->use_global_mask_setting(false);

		this->timer = ref new Timer(this, frame_rate);
		this->timer->stop();

		this->_void = ref new SplitView();
		this->_void->Content = this->canvas;
		this->_void->Pane = this->navigator->user_interface();

		this->_void->OpenPaneLength = 256.0;
		this->_void->DisplayMode = SplitViewDisplayMode::Overlay;
		
		this->_void->PointerReleased += ref new PointerEventHandler(this, &TimeMachineUniverseDisplay::on_pointer_released);
	}

internal:
	void push_timeline(ITimeline* timeline) {
		this->push_planet(timeline);
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
		this->timer->start();
	}

	void on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
		args->Cancel = !(this->machine->can_timemachine_hiding());
	}

	void on_closed(Platform::Object^ target, Platform::Object^ args) {
		this->closed = true;
		this->timer->stop();
		this->machine->on_timemachine_hiden();
		this->global_mask_alpha = this->darkness;
	}

public:
	void travel(long long start_timepoint, long long stop_timepoint) {
		this->timer->stop();
		this->start_timepoint = start_timepoint;
		this->current_timepoint = start_timepoint;
		this->stop_timepoint = stop_timepoint;
		this->timer->start();
	}

	void on_elapse(long long count, long long interval, long long uptime) override {
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
	Timer^ timer;
	double darkness;
	bool closed;

private:
	long long start_timepoint;
	long long current_timepoint;
	long long stop_timepoint;
};

/*************************************************************************************************/
ITimeMachine::ITimeMachine(Platform::String^ dirname, int frame_rate, Syslog* logger
	, Platform::String^ prefix, Platform::String^ suffix, RotationPeriod period, unsigned int period_count)
	: IRotativeDirectory(dirname, prefix, suffix, period, period_count), ready(false) {
	TimeMachineUniverseDisplay^ _universe = ref new TimeMachineUniverseDisplay(logger, this, frame_rate);

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

void ITimeMachine::push_timeline(ITimeline* timeline) {
	auto tmud = dynamic_cast<TimeMachineUniverseDisplay^>(this->universe);

	if (tmud != nullptr) {
		tmud->push_timeline(timeline);
		this->timelines.push_back(timeline);
	}
}

void ITimeMachine::travel(long long start_timepoint, long long stop_timepoint) {
	auto tmud = dynamic_cast<TimeMachineUniverseDisplay^>(this->universe);

	if (tmud != nullptr) {
		tmud->travel(start_timepoint, stop_timepoint);
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

/*************************************************************************************************/
TimeMachine::TimeMachine(Platform::String^ dirname, int frame_rate, Syslog* logger
	, Platform::String^ file_prefix, Platform::String^ file_suffix, RotationPeriod period, unsigned int period_count)
	: ITimeMachine(dirname, frame_rate, logger, file_prefix, file_suffix, period, period_count) {}

void TimeMachine::on_file_rotated(StorageFile^ prev_file, StorageFile^ current_file, long long timepoint) {
	if (prev_file != nullptr) {
		this->tmstream.close();
	}
	
	// TODO: find the reason if `open` fails.
	this->tmstream.open(current_file->Path->Data(), std::ios::out | std::ios::app);
}

void TimeMachine::snapshot(long long timepoint_s, size_t addr0, size_t addrn, const char* data, size_t size) {
	// TODO: find the reason if `write` fails.
	if (this->tmstream.is_open()) {
		this->tmstream << timepoint_s << " " << addr0 << " " << addrn << std::endl;
		this->tmstream.write(data, size) << std::endl;
		this->tmstream.flush();
	}
}
