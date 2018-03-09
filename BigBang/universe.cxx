#include "transformation.hpp"
#include "universe.hxx"
#include "planet.hpp"
#include "syslog.hpp"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Windows::UI;
using namespace Windows::UI::Core;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

#define PLANET_INFO(planet) (static_cast<PlanetInfo*>(planet->info))
#define SHIFTED(vkms) ((vkms & VirtualKeyModifiers::Shift) == VirtualKeyModifiers::Shift)
#define CONTROLLED(vkms) ((vkms & VirtualKeyModifiers::Control) == VirtualKeyModifiers::Control)

class PlanetInfo : public WarGrey::SCADA::IPlanetInfo {
public:
	PlanetInfo(IDisplay^ master) : IPlanetInfo(master) {};

public:
	IPlanet* next;
	IPlanet* prev;
};

static inline PlanetInfo* bind_planet_owership(IDisplay^ master, IPlanet* planet) {
	auto info = new PlanetInfo(master);
	
	planet->info = info;

	return info;
}

static void draw_planet(CanvasDrawingSession^ ds, IPlanet* planet, float width, float height, Syslog* logger) {
	try {
		planet->enter_shared_section();
		planet->draw(ds, width, height);
		planet->leave_shared_section();
	} catch (Platform::Exception^ wte) {
		planet->leave_shared_section();
		logger->log_message(Log::Warning, L"%s: rendering: %s", planet->name()->Data(), wte->Message->Data());
	}
}

/*************************************************************************************************/
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

void IDisplay::enter_critical_section() {
	this->section.lock();
}

void IDisplay::leave_critical_section() {
	this->section.unlock();
}

/*************************************************************************************************/
UniverseDisplay::UniverseDisplay(int frame_rate, Syslog* logger) {
	this->logger = ((logger == nullptr) ? make_silent_logger("UniverseDisplay") : logger);
	this->logger->reference();

	this->navigator_view = ref new ListView();
	this->navigator_view->SelectionMode = ListViewSelectionMode::Single;
	this->navigator_view->IsItemClickEnabled = true;
	this->navigator_view->ItemClick += ref new ItemClickEventHandler(this, &UniverseDisplay::do_transfer);

	this->transfer_clock = ref new DispatcherTimer();
	this->transfer_clock->Tick += ref new EventHandler<Object^>(this, &UniverseDisplay::do_refresh);

	this->display = ref new CanvasAnimatedControl();
	this->display->Name = this->logger->get_name();
	this->display->TargetElapsedTime = make_timespan_from_rate(frame_rate);
	this->display->UseSharedDevice = true; // this is required

	/** TODO
	 * All these events should be unloaded,
	 *  however this control has the same lifetime with the application,
	 *  currently there is no such code for unloading.
	 */
	this->display->SizeChanged += ref new SizeChangedEventHandler(this, &UniverseDisplay::do_resize);
	this->display->CreateResources += ref new UniverseLoadHandler(this, &UniverseDisplay::do_construct);
	this->display->GameLoopStarting += ref new UniverseHandler(this, &UniverseDisplay::do_start);
	this->display->GameLoopStopped += ref new UniverseHandler(this, &UniverseDisplay::do_stop);
	this->display->Update += ref new UniverseUpdateHandler(this, &UniverseDisplay::do_update);
	this->display->Draw += ref new UniverseDrawHandler(this, &UniverseDisplay::do_paint);

	this->display->ManipulationMode = ManipulationModes::TranslateX;
	this->display->PointerPressed += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_pressed);
	this->display->PointerMoved += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moved);
	this->display->ManipulationCompleted += ref new ManipulationCompletedEventHandler(this, &UniverseDisplay::on_maniplated);
	this->display->PointerReleased += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_released);
}

UniverseDisplay::~UniverseDisplay() {
	this->collapse();
	this->transfer_clock->Stop();
	this->logger->destroy();
}

Syslog* UniverseDisplay::get_logger() {
	return this->logger;
}

UIElement^ UniverseDisplay::navigator::get() {
	return this->navigator_view;
}

UserControl^ UniverseDisplay::canvas::get() {
	return this->display;
}

CanvasDevice^ UniverseDisplay::device::get() {
	return this->display->Device;
}

float UniverseDisplay::actual_width::get() {
	return float(this->display->Size.Width);
}

float UniverseDisplay::actual_height::get() {
	return float(this->display->Size.Height);
}

void UniverseDisplay::add_planet(IPlanet* planet) {
	// NOTE: this method is designed to be invoked before CreateResources event

	if (planet->info == nullptr) {
		PlanetInfo* info = bind_planet_owership(this, planet);
		Platform::Object^ label = planet->navigation_label();

		if (this->head_planet == nullptr) {
			this->head_planet = planet;
			this->current_planet = planet;
			info->prev = this->head_planet;
			
			this->navigator_view->Items->Append(label);
			this->navigator_view->SelectedValue = label;

			this->logger->log_message(Log::Debug, L"found the first planet[%s]", planet->name()->Data());
		} else { 
			PlanetInfo* head_info = PLANET_INFO(this->head_planet);
			PlanetInfo* prev_info = PLANET_INFO(head_info->prev);
			 
			info->prev = head_info->prev;
			prev_info->next = planet;
			head_info->prev = planet;

			this->navigator_view->Items->Append(label);

			this->logger->log_message(Log::Debug, L"found another planet[%s]", planet->name()->Data());
		}

		info->next = this->head_planet;
	}
}

void UniverseDisplay::transfer(int delta_idx, unsigned int ms, unsigned int count) {
	if ((this->current_planet != nullptr) && (delta_idx != 0)) {
		bool is_animated = ((!this->transfer_clock->IsEnabled) && ((ms * count) > 0));

		if (is_animated && (this->from_planet == nullptr)) {
			// thread-safe is granteed for `from_planet`
			this->from_planet = this->current_planet;
		}

		this->enter_critical_section();
		if (delta_idx > 0) {
			for (int i = 0; i < delta_idx; i++) {
				this->current_planet = PLANET_INFO(this->current_planet)->next;
			}
		} else {
			for (int i = 0; i > delta_idx; i--) {
				this->current_planet = PLANET_INFO(this->current_planet)->prev;
			}
		}
		this->leave_critical_section();

		this->navigator_view->SelectedValue = this->current_planet->navigation_label();

		if (is_animated) {
			TimeSpan ts = make_timespan_from_ms(ms);
			float width = this->display->Size.Width;

			ts.Duration = ms / count;
			this->transfer_clock->Interval = ts;
			this->transfer_delta = width / float(count) * ((delta_idx > 0) ? -1.0F : 1.0F);
			this->transferX = this->transfer_delta;
			this->transfer_clock->Start();
		} else if (this->from_planet != nullptr) {
			this->logger->log_message(Log::Debug, L"transferred immediately: %s ==> %s",
				this->from_planet->name()->Data(),
				this->current_planet->name()->Data());
			this->from_planet = nullptr;
		}
	}
}

void UniverseDisplay::transfer_previous(unsigned int ms, unsigned int count) {
	this->transfer(-1, ms, count);
}

void UniverseDisplay::transfer_next(unsigned int ms, unsigned int count) {
	this->transfer(1, ms, count);
}

CanvasRenderTarget^ UniverseDisplay::take_snapshot(float dpi) {
	CanvasRenderTarget^ snapshot = nullptr;

	if (this->current_planet != nullptr) {
		Size region = this->display->Size;

		snapshot = this->current_planet->take_snapshot(region.Width, region.Height, dpi);
	}

	return snapshot;
}

void UniverseDisplay::collapse() {
	if (this->head_planet != nullptr) {
		IPlanet* temp_head = this->head_planet;
		PlanetInfo* temp_info = PLANET_INFO(temp_head);
		PlanetInfo* prev_info = PLANET_INFO(temp_info->prev);

		this->head_planet = nullptr;
		this->current_planet = nullptr;
		prev_info->next = nullptr;

		do {
			IPlanet* child = temp_head;

			temp_head = PLANET_INFO(temp_head)->next;

			delete child; // planet's destructor will delete the associated info object
		} while (temp_head != nullptr);
	}
}

void UniverseDisplay::do_resize(Platform::Object^ sender, SizeChangedEventArgs^ args) {
	if (this->head_planet != nullptr) {
		float nwidth = args->NewSize.Width;
		float nheight = args->NewSize.Height;
		float pwidth = args->PreviousSize.Width;
		float pheight = args->PreviousSize.Height;

		if ((nwidth > 0.0F) && (nheight > 0.0F) && ((nwidth != pwidth) || (nheight != pheight))) {
			IPlanet* child = this->head_planet;
			
			this->logger->log_message(Log::Info, L"resize(%f, %f)", nwidth, nheight);
			
			do {
				PlanetInfo* info = PLANET_INFO(child);

				child->enter_critical_section();
				child->reflow(nwidth, nheight);
				child->leave_critical_section();

				child = info->next;
			} while (child != this->head_planet);
		}
	}
}

void UniverseDisplay::do_start(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	this->logger->log_message(Log::Debug, "big bang");
	this->big_bang();
}

void UniverseDisplay::do_construct(CanvasAnimatedControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	this->logger->log_message(Log::Debug, L"construct planets because of %s", args->Reason.ToString()->Data());
	
	this->construct();
	if (this->head_planet != nullptr) {
		IPlanet* child = this->head_planet;
		Size region = this->display->Size;

		do {
			PlanetInfo* info = PLANET_INFO(child);

			child->construct(args->Reason, region.Width, region.Height);
			child->load(args->Reason, region.Width, region.Height);
			child->reflow(region.Width, region.Height);
			this->logger->log_message(Log::Debug, L"planet[%s] is constructed", child->name()->Data());

			child = info->next;
		} while (child != this->head_planet);
	}
}

void UniverseDisplay::do_update(ICanvasAnimatedControl^ sender, CanvasAnimatedUpdateEventArgs^ args) {
	if (this->head_planet != nullptr) {
		long long count = args->Timing.UpdateCount - 1;
		long long elapsed = args->Timing.ElapsedTime.Duration;
		long long uptime = args->Timing.TotalTime.Duration;
		bool is_slow = args->Timing.IsRunningSlowly;
		IPlanet* child = this->head_planet;

		do {
			child->update(count, elapsed, uptime, is_slow);
			this->logger->log_message(Log::Debug, L"planet[%s] is updated", child->name()->Data());
			child = PLANET_INFO(child)->next;
		} while (child != this->head_planet);

		if (is_slow) {
			this->logger->log_message(Log::Notice, L"the updating spent longer than %fms", float(elapsed) / 10000.0F);
		}
	}
}

void UniverseDisplay::do_paint(ICanvasAnimatedControl^ sender, CanvasAnimatedDrawEventArgs^ args) {
	// NOTE: only the current planet and the one transferred from need to be drawn

	this->enter_critical_section();

	if (this->current_planet != nullptr) {
		CanvasDrawingSession^ ds = args->DrawingSession;
		Size region = this->display->Size;
		float width = region.Width;
		float height = region.Height;

		if (this->from_planet == nullptr) {
			draw_planet(ds, this->current_planet, width, height, this->logger);
			this->logger->log_message(Log::Debug, L"planet[%s] is rendered", this->current_planet->name()->Data());
		} else {
			float deltaX = ((this->transferX < 0.0F) ? width : -width);

			ds->Transform = make_translation_matrix(this->transferX);
			draw_planet(ds, this->from_planet, width, height, this->logger);

			ds->Transform = make_translation_matrix(this->transferX + deltaX);
			draw_planet(ds, this->current_planet, width, height, this->logger);

			if (this->current_planet == this->from_planet) {
				this->logger->log_message(Log::Debug, L"planet[%s] is rendered", this->current_planet->name()->Data());
			} else {
				this->logger->log_message(Log::Debug, L"planet[%s] and planet[%s] are rendered",
					this->from_planet->name()->Data(),
					this->from_planet->name()->Data());
			}
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::do_refresh(Platform::Object^ sender, Platform::Object^ args) {
	const wchar_t* from = this->from_planet->name()->Data();
	const wchar_t* to = this->current_planet->name()->Data();
	float width = this->display->Size.Width;
	float percentage = abs(this->transferX) / width;

	if (percentage < 1.0F) {
		this->logger->log_message(Log::Debug, L"transferring[%.2f%%]: %s ==> %s", percentage * 100.0F, from, to);
		this->display->Invalidate();
		this->transferX += this->transfer_delta;
	} else {
		this->logger->log_message(Log::Debug, L"transferred[%.2f%%]: %s ==> %s", percentage * 100.0F, from, to);
		this->transfer_delta = 0.0F;
		this->transferX = 0.0F;
		this->from_planet = nullptr;
		this->transfer_clock->Stop();
	}
}

void UniverseDisplay::do_transfer(Platform::Object^ sender, ItemClickEventArgs^ args) {
	int from_idx = this->navigator_view->SelectedIndex;
	int delta_idx = 0;

	this->navigator_view->SelectedValue = args->ClickedItem;

	delta_idx = this->navigator_view->SelectedIndex - from_idx;
	if (delta_idx != 0) {
		this->from_planet = this->current_planet;
		this->transfer(this->navigator_view->SelectedIndex - from_idx, 0U);
	}
}

void UniverseDisplay::do_stop(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	this->logger->log_message(Log::Debug, "big rip");
	this->big_rip();
}

void UniverseDisplay::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->canvas->CapturePointer(args->Pointer)) {
		this->enter_critical_section();

		if (this->current_planet != nullptr) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerPointProperties^ ppp = pp->Properties;

			this->saved_pressed_ppp = ppp;
			args->Handled = this->current_planet->on_pointer_pressed(
				pp->Position.X, pp->Position.Y, ppp->PointerUpdateKind,
				SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
		}

		this->leave_critical_section();
	}
}

// TODO: distinguish rubberhand event and maniplation
void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->current_planet != nullptr) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		
		args->Handled = this->current_planet->on_pointer_moved(
			pp->Position.X, pp->Position.Y, args->GetIntermediatePoints(this->canvas),
			pp->Properties->PointerUpdateKind, SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_maniplated(Platform::Object^ sender, ManipulationCompletedRoutedEventArgs^ args) {
	float width = this->actual_width;
	float delta = args->Cumulative.Translation.X;
	float speed = args->Velocities.Linear.X;
	float distance = width * 0.0618F;

	if (delta < -distance) {
		this->transfer_next(256);
	} else if (delta > distance) {
		this->transfer_previous(256);
	}

	args->Handled = true;
}

void UniverseDisplay::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->saved_pressed_ppp != nullptr) {
		this->canvas->ReleasePointerCapture(args->Pointer); // TODO: deal with PointerCaptureLost event;

		this->enter_critical_section();

		if (this->current_planet != nullptr) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerUpdateKind puk = this->saved_pressed_ppp->PointerUpdateKind;

			this->saved_pressed_ppp = nullptr;
			args->Handled = this->current_planet->on_pointer_released(
				pp->Position.X, pp->Position.Y, puk,
				SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
			
		}

		this->leave_critical_section();
	}
}

void UniverseDisplay::on_char(Platform::Object^ sender, KeyRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->current_planet != nullptr) {
		VirtualKey vkey = args->Key;

		args->Handled = this->current_planet->on_char(args->Key);
	}

	this->leave_critical_section();
}
