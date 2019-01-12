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
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class TMIcon : unsigned int { BookMark, Quit, _ };

private enum class TM : unsigned int { Time0, Timen, _ };

static const double time_machine_alpha = 0.64;
static ICanvasBrush^ fg_color = Colours::make(Colours::GhostWhite, time_machine_alpha);

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
	TimeMachineHeadsUp(ITimeMachine* master) : Planet(__MODULE__), master(master) {
		this->datetime_style.datetime_color = fg_color;
		this->datetime_style.label_color = Colours::make(Colours::LightSeaGreen, time_machine_alpha);
		this->datetime_style.caret_color = Colours::make(Colours::DeepSkyBlue, time_machine_alpha);
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		auto icon_font = make_text_format("Consolas", statusbar_height());
		auto icon_color = fg_color;

		for (TMIcon id = _E0(TMIcon); id < TMIcon::_; id++) {
			Platform::String^ caption = nullptr;

			switch (id) {
			case TMIcon::BookMark: caption = L"🔖"; break;
			case TMIcon::Quit: caption = L"🚪"; break;
			}

			this->icons[id] = this->insert_one(new Credit<Labellet, TMIcon>(caption, icon_font, icon_color), id);
		}

		this->load_date_picker(this->time_pickers, TM::Time0, -this->master->span_seconds());
		this->load_date_picker(this->time_pickers, TM::Timen, 0LL);
	}

	void reflow(float width, float height) override {
		float icon_y = statusbar_height() * 0.25F;
		float gapsize = icon_y * 2.0F;
		float icon_rx = width - icon_y;
		float icon_width;

		for (unsigned int idx = _N(TMIcon); idx > 0; idx--) {
			auto icon = this->icons[_E(TMIcon, idx - 1)];

			this->get_logger()->log_message(Log::Info, _E(TMIcon, idx - 1).ToString());
			icon->fill_extent(0.0F, 0.0F, &icon_width);
			this->move_to(icon, icon_rx, icon_y, GraphletAnchor::RT);

			icon_rx -= (icon_width + gapsize);
		}

		this->move_to(this->time_pickers[TM::Time0], this->icons[TMIcon::Quit], GraphletAnchor::RC, GraphletAnchor::LC, icon_y * 2.0F - width);
		this->move_to(this->time_pickers[TM::Timen], this->time_pickers[TM::Time0], GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
	}

public:
	bool can_select(IGraphlet* g) override {
		return ((dynamic_cast<Labellet*>(g) != nullptr)
			|| (dynamic_cast<DatePickerlet*>(g) != nullptr));
	}

	void on_focus(IGraphlet* g) override {
		auto editor = dynamic_cast<DatePickerlet*>(g);

		if (editor != nullptr) {
			this->show_virtual_keyboard(ScreenKeyboard::Bucketpad, GraphletAnchor::CB);
		}
	}

	void on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
		auto icon = dynamic_cast<Credit<Labellet, TMIcon>*>(g);

		if (icon != nullptr) {
			switch (icon->id) {
			case TMIcon::Quit: this->master->hide(); break;
			case TMIcon::BookMark: {
				auto tmud = dynamic_cast<TimeMachineUniverseDisplay^>(this->info->master);

				if (tmud != nullptr) {
					tmud->flyout_content()->IsPaneOpen = true;
				}
			}; break;
			}
		}
	}

private:
	void load_date_picker(std::map<TM, Credit<DatePickerlet, TM>*>& tps, TM id, long long time_offset) {
		tps[id] = this->insert_one(new Credit<DatePickerlet, TM>(DatePickerState::Input, time_offset, _speak(id)), id);
		tps[id]->set_style(this->datetime_style);
	}

private: // never delete this graphlets manually
	std::map<TMIcon, Credit<Labellet, TMIcon>*> icons;
	std::map<TM, Credit<DatePickerlet, TM>*> time_pickers;

private:
	DatePickerStyle datetime_style;

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
