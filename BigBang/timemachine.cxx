#include <list>
#include <map>

#include "timemachine.hpp"

#include "navigator/listview.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"
#include "graphlet/datepickerlet.hpp"

#include "string.hpp"
#include "timer.hxx"
#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Storage;

using namespace Windows::UI::Input;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;

using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum class TimeMachineIcon : unsigned int {
	Quit,
	_
};

/*************************************************************************************************/
private ref class TimeMachineUniverseDisplay sealed : public UniverseDisplay {
internal:
	TimeMachineUniverseDisplay(Syslog* logger, ITimeMachine* entity, int frame_rate, IPlanet* headsup)
		: UniverseDisplay(logger, nullptr, new ListViewNavigator(), headsup)
		, machine(entity), closed(true), current_timepoint(0LL) {
	
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
		this->timelines.push_back(timeline);
	}

public:
	SplitView^ flyout_content() {
		return this->_void;
	}

public:
	void on_opening(Platform::Object^ target, Platform::Object^ args) {
		this->darkness = this->global_mask_alpha;
		this->machine->on_showing();
	}

	void on_opened(Platform::Object^ target, Platform::Object^ args) {
		this->closed = false;
		this->global_mask_alpha = 0.8;
		this->_void->IsPaneOpen = false;
		this->machine->on_shown();
		this->timer->start();
	}

	void on_closing(FlyoutBase^ target, FlyoutBaseClosingEventArgs^ args) {
		args->Cancel = !(this->machine->can_hide());
	}

	void on_closed(Platform::Object^ target, Platform::Object^ args) {
		this->closed = true;
		this->timer->stop();
		this->machine->on_hiden();
		this->global_mask_alpha = this->darkness;
	}

public:
	void travel(long long start_timepoint, long long stop_timepoint) {
		this->timer->stop();

		this->start_timepoint = start_timepoint;
		this->current_timepoint = start_timepoint;
		this->stop_timepoint = stop_timepoint;
		
		if (this->start_timepoint < this->stop_timepoint) {
			this->direction = 1LL;
			this->current_timepoint = this->start_timepoint;
		} else {
			this->direction = -1LL;
			this->current_timepoint = this->stop_timepoint;
		}

		this->timer->start();
	}

	void on_elapse(long long count, long long interval, long long uptime) override {
		
	}

public:
	bool shown() override {
		return !(this->closed);
	}

protected:
	void construct(CanvasCreateResourcesReason reason) override {
		this->machine->construct(reason);
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
	std::list<ITimeline*> timelines; // never deletes these timelines manually
	ITimeMachine* machine; // its lifetime is managed by client applications
	SplitView^ _void;
	Timer^ timer;
	double darkness;
	bool closed;

private:
	long long start_timepoint;
	long long current_timepoint;
	long long stop_timepoint;
	long long direction;
};

private class TimeMachineHeadsUp : public Planet {
public:
	TimeMachineHeadsUp(ITimeMachine* master) : Planet(__MODULE__), master(master) {}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		auto icon_font = make_text_format("Consolas", statusbar_height());
		auto icon_color = Colours::make(Colours::GhostWhite, 0.64);

		for (TimeMachineIcon id = _E0(TimeMachineIcon); id < TimeMachineIcon::_; id++) {
			Platform::String^ caption = nullptr;

			switch (id) {
			//case TimeMachineIcon::PickStartTimePoint: caption = L"⏳"; break;
			//case TimeMachineIcon::PickStopTimePoint: caption = L"⌛️"; break;
			case TimeMachineIcon::Quit: caption = L"🚪"; break;
			}

			this->icons[id] = this->insert_one(new Credit<Labellet, TimeMachineIcon>(caption, icon_font, icon_color), id);
		}
	}

	void reflow(float width, float height) override {
		float icon_y = statusbar_height() * 0.25F;
		float gapsize = icon_y * 2.0F;
		float icon_rx = width - icon_y;
		float icon_width;

		for (unsigned int idx = _N(TimeMachineIcon); idx > 0; idx--) {
			auto icon = this->icons[_E(TimeMachineIcon, idx - 1)];

			this->get_logger()->log_message(Log::Info, _E(TimeMachineIcon, idx - 1).ToString());
			icon->fill_extent(0.0F, 0.0F, &icon_width);
			this->move_to(icon, icon_rx, icon_y, GraphletAnchor::RT);

			icon_rx -= (icon_width + gapsize);
		}
	}

public:
	bool can_select(IGraphlet* g) override {
		return (dynamic_cast<Labellet*>(g) != nullptr);
	}

	void on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
		auto icon = dynamic_cast<Credit<Labellet, TimeMachineIcon>*>(g);

		if (icon != nullptr) {
			switch (icon->id) {
			case TimeMachineIcon::Quit: this->master->hide(); break;
			//case TimeMachineIcon::PickStartTimePoint: case TimeMachineIcon::PickStopTimePoint: {
				
			//}; break;
			}
		}
	}

private: // never delete this graphlets manually
	std::map<TimeMachineIcon, Credit<Labellet, TimeMachineIcon>*> icons;
	DatePickerlet* start_time_picker;
	DatePickerlet* stop_time_picker;

private:
	ITimeMachine* master;
};

/*************************************************************************************************/
ITimeMachine::ITimeMachine(Platform::String^ dirname, int frame_rate, Syslog* logger
	, Platform::String^ prefix, Platform::String^ suffix, RotationPeriod period, unsigned int period_count)
	: IRotativeDirectory(dirname, prefix, suffix, period, period_count), ready(false) {
	TimeMachineUniverseDisplay^ _universe = ref new TimeMachineUniverseDisplay(logger, this, frame_rate, new TimeMachineHeadsUp(this));

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
	}
}

void ITimeMachine::travel(long long start_timepoint, long long stop_timepoint) {
	auto tmud = dynamic_cast<TimeMachineUniverseDisplay^>(this->universe);

	if (tmud != nullptr) {
		tmud->travel(start_timepoint, stop_timepoint);
	}
}

Syslog* ITimeMachine::get_logger() {
	return this->universe->get_logger();
}

Flyout^ ITimeMachine::user_interface() {
	return this->machine;
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

void TimeMachine::save_snapshot(long long timepoint_s, size_t addr0, size_t addrn, const char* data, size_t size) {
	// TODO: find the reason if `write` fails.
	if (this->tmstream.is_open()) {
		this->tmstream << timepoint_s << " " << addr0 << " " << addrn << std::endl;
		this->tmstream.write(data, size) << std::endl;
		this->tmstream.flush();
	}
}

const char* TimeMachine::seek_snapshot(long long* timepoint_ms, size_t* addr0, size_t* addrn) {
	return nullptr;
}
