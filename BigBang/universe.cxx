#include "transformation.hpp"
#include "universe.hxx"
#include "planet.hpp"
#include "system.hpp"
#include "syslog.hpp"
#include "time.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Windows::Devices::Input;

using namespace Windows::UI;
using namespace Windows::UI::Core;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;
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
	planet->enter_shared_section();
	
	try {
		planet->draw(ds, width, height);
	} catch (Platform::Exception^ wte) {
		logger->log_message(Log::Warning, L"%s: rendering: %s", planet->name()->Data(), wte->Message->Data());
	}

	planet->leave_shared_section();
}

static inline float display_contain_mode_scale(float to_width, float to_height, float from_width, float from_height) {
	return std::fminf(std::fminf(to_width / from_width, to_height / from_height), 1.0F);
}

/*************************************************************************************************/
IDisplay::IDisplay(Syslog* logger, DisplayFit mode, float dest_width, float dest_height, float src_width, float src_height)
	: logger((logger == nullptr) ? make_silent_logger("IDisplay") : logger), mode(mode)
	, target_width(std::fmaxf(dest_width, 0.0F)), target_height(std::fmaxf(dest_height, 0.0F))
	, source_width(src_width), source_height(src_height) {
	this->logger->reference();

	if (this->source_width <= 0.0F) {
		this->source_width = this->target_width;
	}

	if (this->source_height <= 0.0F) {
		this->source_height = this->target_height;
	}

	if ((this->target_width * this->target_height) == 0.0F) {
		this->mode = DisplayFit::None;
	}
}

IDisplay::~IDisplay() {
	this->logger->destroy();
}

bool IDisplay::shown() {
	// TODO: The VisualTree should get involved
	return true;
}

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

void IDisplay::apply_source_size(float src_width, float src_height) {
	this->width = this->sketch_to_application_width(src_width);
	this->height = this->sketch_to_application_height(src_height);
}

float IDisplay::sketch_to_application_width(float sketch_width) {
	static Size screen = system_screen_size();
	float width = sketch_width;

	switch (this->mode) {
	case DisplayFit::Contain: {
		static float scale = display_contain_mode_scale(screen.Width, screen.Height, target_width, target_height);

		width = (sketch_width / this->source_width * this->target_width) * scale;
	}; break;
	case DisplayFit::Fill: {
		width = sketch_width * screen.Width / this->source_width;
	}; break;
	}

	return width;
}

float IDisplay::sketch_to_application_height(float sketch_height) {
	static Size screen = system_screen_size();
	float height = sketch_height;

	switch (this->mode) {
	case DisplayFit::Contain: {
		static float scale = display_contain_mode_scale(screen.Width, screen.Height, target_width, target_height);

		height = (sketch_height / this->source_height * this->target_height) * scale;
	}; break;
	case DisplayFit::Fill: {
		height = sketch_height * screen.Height / this->source_height;
	}; break;
	}

	return height;
}

void IDisplay::enter_critical_section() {
	this->section.lock();
}

void IDisplay::leave_critical_section() {
	this->section.unlock();
}

Syslog* IDisplay::get_logger() {
	return this->logger;
}

/*************************************************************************************************/
UniverseDisplay::UniverseDisplay(Syslog* logger, IPlanet* first_planet, ListView^ navigator)
	: UniverseDisplay(DisplayFit::None, 0.0F, 0.0F, logger, first_planet, navigator) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight, Syslog* logger, IPlanet* first_planet, ListView^ navigator)
	: UniverseDisplay(mode, dwidth, dheight, dwidth, dheight, logger, first_planet, navigator) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight, float swidth, float sheight, Syslog* logger, IPlanet* first_planet, ListView^ navigator)
	: IDisplay(((logger == nullptr) ? make_silent_logger("UniverseDisplay") : logger), mode, dwidth, dheight, swidth, sheight) {
	this->navigator_view = ((navigator == nullptr) ? ref new ListView() : navigator);
	this->navigator_view->SelectionMode = ListViewSelectionMode::Single;
	this->navigator_view->IsItemClickEnabled = true;
	this->navigator_view->ItemClick += ref new ItemClickEventHandler(this, &UniverseDisplay::do_transfer);

	this->transfer_clock = ref new DispatcherTimer();
	this->transfer_clock->Tick += ref new EventHandler<Object^>(this, &UniverseDisplay::do_refresh);

	this->display = ref new CanvasControl();
	this->display->Name = this->get_logger()->get_name();
	
	// CanvasControl uses the shared one by default, while CanvasAnimatedControl is not.
	// this->display->UseSharedDevice = true;

	if (first_planet != nullptr) {
		this->add_planet(first_planet);
	}

	/** TODO
	 * All these events should be unloaded,
	 *  however this control has the same lifetime with the application,
	 *  currently there is no such code for unloading.
	 */
	this->display->SizeChanged += ref new SizeChangedEventHandler(this, &UniverseDisplay::do_resize);
	this->display->CreateResources += ref new UniverseLoadHandler(this, &UniverseDisplay::do_construct);
	this->display->Draw += ref new UniverseDrawHandler(this, &UniverseDisplay::do_paint);

	this->display->ManipulationMode = ManipulationModes::TranslateX;
	this->display->PointerPressed += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_pressed);
	this->display->PointerMoved += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moved);
	this->display->ManipulationCompleted += ref new ManipulationCompletedEventHandler(this, &UniverseDisplay::on_maniplated);
	this->display->PointerReleased += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_released);
	this->display->PointerExited += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moveout);
}

UniverseDisplay::~UniverseDisplay() {
	this->collapse();
	this->transfer_clock->Stop();
}

Selector^ UniverseDisplay::navigator::get() {
	return this->navigator_view;
}

IPlanet* UniverseDisplay::current_planet::get() {
	return this->recent_planet;
}

unsigned int UniverseDisplay::current_planet_index::get() {
	return (unsigned int)this->navigator_view->SelectedIndex;
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

bool UniverseDisplay::ui_thread_ready() {
	return this->display->Dispatcher->HasThreadAccess;
}

bool UniverseDisplay::surface_ready() {
	return (this->shown() && this->display->ReadyToDraw);
}

void UniverseDisplay::refresh(IPlanet* which) {
	if (this->recent_planet == which) {
		this->display->Invalidate();
	}
}

void UniverseDisplay::on_elapsed(long long count, long long elapsed, long long uptime) {
	if (this->head_planet != nullptr) {
		IPlanet* child = PLANET_INFO(this->recent_planet)->next;
		
		this->recent_planet->begin_update_sequence();
		this->recent_planet->update(count, elapsed, uptime);
		this->recent_planet->end_update_sequence();

		while (child != this->recent_planet) {
			child->update(count, elapsed, uptime);
			child = PLANET_INFO(child)->next;
		}
	}
}

void UniverseDisplay::add_planet(IPlanet* planet) {
	// NOTE: this method is designed to be invoked before CreateResources event

	if (planet->info == nullptr) {
		PlanetInfo* info = bind_planet_owership(this, planet);
		Platform::Object^ label = planet->navigation_label();

		if (this->head_planet == nullptr) {
			this->head_planet = planet;
			this->recent_planet = planet;
			info->prev = this->head_planet;
			
			this->navigator_view->Items->Append(label);
			this->navigator_view->SelectedValue = label;

			this->get_logger()->log_message(Log::Debug, L"found the first planet[%s]", planet->name()->Data());
		} else { 
			PlanetInfo* head_info = PLANET_INFO(this->head_planet);
			PlanetInfo* prev_info = PLANET_INFO(head_info->prev);
			 
			info->prev = head_info->prev;
			prev_info->next = planet;
			head_info->prev = planet;

			this->navigator_view->Items->Append(label);

			this->get_logger()->log_message(Log::Debug, L"found another planet[%s]", planet->name()->Data());
		}

		info->next = this->head_planet;
	}
}

void UniverseDisplay::transfer(int delta_idx, unsigned int ms, unsigned int count) {
	if ((this->recent_planet != nullptr) && (delta_idx != 0)) {
		bool is_animated = ((!this->transfer_clock->IsEnabled) && ((ms * count) > 0));

		if (is_animated && (this->from_planet == nullptr)) {
			// thread-safe is granteed for `from_planet`
			this->from_planet = this->recent_planet;
		}

		this->enter_critical_section();
		if (delta_idx > 0) {
			for (int i = 0; i < delta_idx; i++) {
				this->recent_planet = PLANET_INFO(this->recent_planet)->next;
			}
		} else {
			for (int i = 0; i > delta_idx; i--) {
				this->recent_planet = PLANET_INFO(this->recent_planet)->prev;
			}
		}
		this->leave_critical_section();

		this->navigator_view->SelectedValue = this->recent_planet->navigation_label();

		if (is_animated) {
			TimeSpan ts = make_timespan_from_ms(ms);
			float width = this->display->Size.Width;

			ts.Duration = ms / count;
			this->transfer_clock->Interval = ts;
			this->transfer_delta = width / float(count) * ((delta_idx > 0) ? -1.0F : 1.0F);
			this->transferX = this->transfer_delta;
			this->transfer_clock->Start();
		} else if (this->from_planet != nullptr) {
			this->get_logger()->log_message(Log::Debug, L"transferred immediately: %s ==> %s",
				this->from_planet->name()->Data(),
				this->recent_planet->name()->Data());
			this->from_planet = nullptr;
			this->refresh(this->recent_planet);
		}
	}
}

void UniverseDisplay::transfer_to(int idx, unsigned int ms, unsigned int count) {
	int from_idx = this->navigator_view->SelectedIndex;
	int delta_idx = idx - from_idx;

	if (delta_idx != 0) {
		this->from_planet = this->recent_planet;
		this->transfer(delta_idx, 0U);
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

	if (this->recent_planet != nullptr) {
		Size region = this->display->Size;

		snapshot = this->recent_planet->take_snapshot(region.Width, region.Height, nullptr, dpi);
	}

	return snapshot;
}

void UniverseDisplay::collapse() {
	if (this->head_planet != nullptr) {
		IPlanet* temp_head = this->head_planet;
		PlanetInfo* temp_info = PLANET_INFO(temp_head);
		PlanetInfo* prev_info = PLANET_INFO(temp_info->prev);

		this->head_planet = nullptr;
		this->recent_planet = nullptr;
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
			
			this->get_logger()->log_message(Log::Debug, L"resize(%f, %f)", nwidth, nheight);
			
			do {
				PlanetInfo* info = PLANET_INFO(child);

				if (child->surface_ready()) {
					child->enter_critical_section();
					child->reflow(nwidth, nheight);
					child->leave_critical_section();
				}

				child = info->next;
			} while (child != this->head_planet);
		}
	}
}

void UniverseDisplay::do_construct(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	this->get_logger()->log_message(Log::Debug, L"construct planets because of %s", args->Reason.ToString()->Data());
	
	this->construct();
	if (this->head_planet != nullptr) {
		IPlanet* child = this->head_planet;
		Size region = this->display->Size;

		do {
			PlanetInfo* info = PLANET_INFO(child);

			child->begin_update_sequence();

			try {
				child->construct(args->Reason, region.Width, region.Height);
				child->load(args->Reason, region.Width, region.Height);
				child->reflow(region.Width, region.Height);
				child->notify_surface_ready();

				this->get_logger()->log_message(Log::Debug, L"planet[%s] is constructed in region[%f, %f]",
					child->name()->Data(), region.Width, region.Height);
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Critical, L"%s: constructing: %s",
					child->name()->Data(), e->Message->Data());
			}

			child->end_update_sequence();

			child = info->next;
		} while (child != this->head_planet);
	}
}

void UniverseDisplay::do_paint(CanvasControl^ sender, CanvasDrawEventArgs^ args) {
	// NOTE: only the current planet and the one transferred from need to be drawn

	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		CanvasDrawingSession^ ds = args->DrawingSession;
		Size region = this->display->Size;
		float width = region.Width;
		float height = region.Height;

		if (this->from_planet == nullptr) {
			draw_planet(ds, this->recent_planet, width, height, this->get_logger());
		} else {
			float deltaX = ((this->transferX < 0.0F) ? width : -width);

			ds->Transform = make_translation_matrix(this->transferX);
			draw_planet(ds, this->from_planet, width, height, this->get_logger());

			ds->Transform = make_translation_matrix(this->transferX + deltaX);
			draw_planet(ds, this->recent_planet, width, height, this->get_logger());
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::do_refresh(Platform::Object^ sender, Platform::Object^ args) {
	const wchar_t* from = this->from_planet->name()->Data();
	const wchar_t* to = this->recent_planet->name()->Data();
	float width = this->display->Size.Width;
	float percentage = abs(this->transferX) / width;

	if (percentage < 1.0F) {
		this->get_logger()->log_message(Log::Debug, L"transferring[%.2f%%]: %s ==> %s", percentage * 100.0F, from, to);
		this->display->Invalidate();
		this->transferX += this->transfer_delta;
	} else {
		this->get_logger()->log_message(Log::Debug, L"transferred[%.2f%%]: %s ==> %s", percentage * 100.0F, from, to);
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
		this->from_planet = this->recent_planet;
		this->transfer(delta_idx, 0U);
	}
}

void UniverseDisplay::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->canvas->CapturePointer(args->Pointer)) {
		this->enter_critical_section();

		if (this->recent_planet != nullptr) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			PointerPointProperties^ ppp = pp->Properties;

			this->saved_pressed_ppp = ppp;
			args->Handled = this->recent_planet->on_pointer_pressed(
				pp->Position.X, pp->Position.Y, pdt, ppp->PointerUpdateKind,
				SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
		}

		this->leave_critical_section();
	}
}

// TODO: distinguish rubberhand event and maniplation
void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;
		
		args->Handled = this->recent_planet->on_pointer_moved(
			pp->Position.X, pp->Position.Y,
			args->GetIntermediatePoints(this->canvas),
			pdt, pp->Properties->PointerUpdateKind,
			SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
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

		if (this->recent_planet != nullptr) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			PointerUpdateKind puk = this->saved_pressed_ppp->PointerUpdateKind;

			this->saved_pressed_ppp = nullptr;
			args->Handled = this->recent_planet->on_pointer_released(
				pp->Position.X, pp->Position.Y, pdt, puk,
				SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
			
		}

		this->leave_critical_section();
	}
}

void UniverseDisplay::on_pointer_moveout(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;

		args->Handled = this->recent_planet->on_pointer_moveout(
			pp->Position.X, pp->Position.Y,
			pdt, pp->Properties->PointerUpdateKind,
			SHIFTED(args->KeyModifiers), CONTROLLED(args->KeyModifiers));
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_char(Platform::Object^ sender, KeyRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		VirtualKey vkey = args->Key;

		if (this->controlling) {
			switch (vkey) {
			case VirtualKey::S: {
				this->recent_planet->save(
					ms_apptemp_file(this->recent_planet->name(), ".png"),
					this->actual_width, this->actual_height);
			}; break;
			case VirtualKey::I: {
				float x, y, width, height;

				this->recent_planet->fill_graphlets_boundary(&x, &y, &width, &height);
				this->recent_planet->save(ms_apptemp_file("icon", ".png"), x, y, width, height);
			}; break;
			}
		}

		this->controlling = (vkey == VirtualKey::Control);

		args->Handled = this->recent_planet->on_char(args->Key, false);
	}

	this->leave_critical_section();
}
