#include "universe.hxx"
#include "planet.hpp"
#include "navigator/null.hpp"

#include "system.hpp"
#include "syslog.hpp"
#include "time.hpp"
#include "path.hpp"

#include "paint.hpp"
#include "colorspace.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Windows::Devices::Input;
using namespace Windows::Storage;

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
#define WINDOWED(vkms) ((vkms & VirtualKeyModifiers::Windows) == VirtualKeyModifiers::Windows)
#define MENUED(vkms) ((vkms & VirtualKeyModifiers::Menu) == VirtualKeyModifiers::Menu)

static Platform::String^ page_setting_key = "Page_Name";
static Platform::String^ mask_setting_key = "Mask_Alpha";

static CanvasSolidColorBrush^ global_mask_color;

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

static inline void set_mask_alpha(CanvasSolidColorBrush^ color, double v) {
	color->Color = rgba(color->Color, v);
}

static inline double get_mask_alpha(CanvasSolidColorBrush^ color) {
	return double(color->Color.A) / 255.0;
}

static inline void load_mask_alpha(ApplicationDataContainer^ container, CanvasSolidColorBrush^ color) {
	if (container->Values->HasKey(mask_setting_key)) {
		set_mask_alpha(color, double(container->Values->Lookup(mask_setting_key)));
	}
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
UniverseDisplay::UniverseDisplay(Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IPlanet* first_planet)
	: UniverseDisplay(DisplayFit::None, 0.0F, 0.0F, logger, setting_name, navigator, first_planet) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight
	, Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IPlanet* first_planet)
	: UniverseDisplay(mode, dwidth, dheight, dwidth, dheight, logger, setting_name, navigator, first_planet) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight, float swidth, float sheight
	, Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IPlanet* first_planet)
	: IDisplay(((logger == nullptr) ? make_silent_logger("UniverseDisplay") : logger), mode, dwidth, dheight, swidth, sheight)
	, figure_x0(std::nanf("swipe")), universe_settings(nullptr), shortcuts_enabled(true), follow_global_mask_setting(true) {
	this->transfer_clock = ref new DispatcherTimer();
	this->transfer_clock->Tick += ref new EventHandler<Platform::Object^>(this, &UniverseDisplay::do_refresh);

	this->_navigator = ((navigator == nullptr) ? new NullNavigator() : navigator);
	this->_navigator->push_navigation_listener(this);

	if (setting_name != nullptr) {
		ApplicationDataCreateDisposition adcd = ApplicationDataCreateDisposition::Always;

		this->universe_settings = ApplicationData::Current->LocalSettings->CreateContainer(setting_name, adcd);
	}

	this->display = ref new CanvasControl();
	this->display->Name = this->get_logger()->get_name();
	
	// CanvasControl uses the shared one by default, while CanvasAnimatedControl is not.
	// this->display->UseSharedDevice = true;

	if (first_planet != nullptr) {
		this->push_planet(first_planet);
	}

	{ // initialize gesture
	    /** TODO
		 * All these events should be unloaded,
		 *  however this control has the same lifetime with the application,
		 *  currently there is no such code for unloading.
		 */
		this->display->SizeChanged += ref new SizeChangedEventHandler(this, &UniverseDisplay::do_resize);
		this->display->CreateResources += ref new UniverseLoadHandler(this, &UniverseDisplay::do_construct);
		this->display->Draw += ref new UniverseDrawHandler(this, &UniverseDisplay::do_paint);

		this->display->PointerPressed += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_pressed);
		this->display->PointerMoved += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moved);
		this->display->PointerReleased += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_released);
		this->display->PointerExited += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moveout);

		CoreWindow::GetForCurrentThread()->CharacterReceived +=
			ref new TypedEventHandler<CoreWindow^, CharacterReceivedEventArgs^>(this, &UniverseDisplay::on_character);
	}

	{ // initialize masks
		this->mask_color = make_solid_brush(0x000000, 0.0F);

		if (global_mask_color == nullptr) {
			global_mask_color = make_solid_brush(0x000000, 0.0F);
		}

		load_mask_alpha(ApplicationData::Current->LocalSettings, global_mask_color);

		if (this->universe_settings != nullptr) {
			load_mask_alpha(this->universe_settings, this->mask_color);
		}
	}
}

void UniverseDisplay::register_virtual_keydown_event_handler(UIElement^ target) {
	target->KeyDown += ref new KeyEventHandler(this, &UniverseDisplay::on_key);
}

UniverseDisplay::~UniverseDisplay() {
	this->transfer_clock->Stop();
	this->collapse();
	
	if (this->_navigator != nullptr) {
		delete this->_navigator;
	}
}

IUniverseNavigator* UniverseDisplay::navigator::get() {
	return this->_navigator;
}

IPlanet* UniverseDisplay::current_planet::get() {
	return this->recent_planet;
}

int UniverseDisplay::current_planet_index::get() {
	return this->_navigator->selected_index();
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

void UniverseDisplay::global_mask_alpha::set(double v) {
	ApplicationData::Current->LocalSettings->Values->Insert(mask_setting_key, v);
	set_mask_alpha(global_mask_color, v);
}

double UniverseDisplay::global_mask_alpha::get() {
	return get_mask_alpha(global_mask_color);
}

void UniverseDisplay::mask_alpha::set(double v) {
	set_mask_alpha(this->mask_color, v);

	if (this->universe_settings != nullptr) {
		this->universe_settings->Values->Insert(mask_setting_key, v);
	}
}

double UniverseDisplay::mask_alpha::get() {
	return get_mask_alpha(this->mask_color);
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

void UniverseDisplay::on_elapse(long long count, long long interval, long long uptime) {
	if (this->head_planet != nullptr) {
		IPlanet* child = PLANET_INFO(this->recent_planet)->next;
		
		this->recent_planet->begin_update_sequence();
		this->recent_planet->on_elapse(count, interval, uptime);
		this->recent_planet->end_update_sequence();

		while (child != this->recent_planet) {
			child->on_elapse(count, interval, uptime);
			child = PLANET_INFO(child)->next;
		}
	}

	this->update(count, interval, uptime);
}

void UniverseDisplay::on_elapse(long long count, long long interval, long long uptime, long long elapsed) {
	if (this->head_planet != nullptr) {
		IPlanet* child = PLANET_INFO(this->recent_planet)->next;

		this->recent_planet->begin_update_sequence();
		this->recent_planet->on_elapse(count, interval, uptime, elapsed);
		this->recent_planet->end_update_sequence();

		while (child != this->recent_planet) {
			child->on_elapse(count, interval, uptime, elapsed);
			child = PLANET_INFO(child)->next;
		}
	}
}

void UniverseDisplay::push_planet(IPlanet* planet) {
	// NOTE: this method is designed to be invoked before CreateResources event

	if (planet->info == nullptr) {
		PlanetInfo* info = bind_planet_owership(this, planet);
		
		if (this->head_planet == nullptr) {
			this->head_planet = planet;
			this->recent_planet = planet;
			info->prev = this->head_planet;
			
			this->_navigator->insert(planet);
			this->_navigator->select(planet);

			this->get_logger()->log_message(Log::Debug, L"found the first planet[%s]", planet->name()->Data());
		} else { 
			PlanetInfo* head_info = PLANET_INFO(this->head_planet);
			PlanetInfo* prev_info = PLANET_INFO(head_info->prev);
			 
			info->prev = head_info->prev;
			prev_info->next = planet;
			head_info->prev = planet;

			this->_navigator->insert(planet);

			this->get_logger()->log_message(Log::Debug, L"found another planet[%s]", planet->name()->Data());
		}

		info->next = this->head_planet;

		if (this->universe_settings != nullptr) {
			if (!this->universe_settings->Values->HasKey(page_setting_key)) {
				this->universe_settings->Values->Insert(page_setting_key, planet->name());
			}
		}
	}
}

void UniverseDisplay::transfer(int delta_idx, unsigned int ms, unsigned int count) {
	if ((this->recent_planet != nullptr) && (delta_idx != 0) && (!this->transfer_clock->IsEnabled)) {
		bool animating = ((ms * count) > 0);

		if (animating && (this->from_planet == nullptr)) {
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

		this->_navigator->select(this->recent_planet);

		if (this->universe_settings != nullptr) {
			this->universe_settings->Values->Insert(page_setting_key, this->recent_planet->name());
		}

		if (animating) {
			TimeSpan ts = make_timespan_from_milliseconds(ms);
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

void UniverseDisplay::transfer_to(Platform::String^ name, unsigned int ms, unsigned int count) {
	int index = -1;
	
	if ((this->head_planet != nullptr) && (name != nullptr)) {
		IPlanet* child = this->head_planet;
		int idx = 0;

		do {
			PlanetInfo* info = PLANET_INFO(child);

			if (child->name()->Equals(name)) {
				index = idx;
				break;
			}

			idx += 1;
			child = info->next;
		} while (child != this->head_planet);
	}

	if (index >= 0) {
		this->transfer_to(index, ms, count);
	}
}

void UniverseDisplay::transfer_to(int idx, unsigned int ms, unsigned int count) {
	int from_idx = this->_navigator->selected_index();
	int delta_idx = idx - from_idx;

	if (delta_idx != 0) {
		this->from_planet = this->recent_planet;
		this->transfer(delta_idx, ms, count);
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
		Platform::String^ last_page = nullptr;

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

	if (this->universe_settings != nullptr) {
		this->transfer_to(this->universe_settings->Values->Lookup(page_setting_key)->ToString());
	}
}

void UniverseDisplay::do_paint(CanvasControl^ sender, CanvasDrawEventArgs^ args) {
	CanvasDrawingSession^ ds = args->DrawingSession;
	Size region = this->display->Size;
	float width = region.Width;
	float height = region.Height;

	// NOTE: only the current planet and the one transferred from need to be drawn

	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
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
	
	{ // draw mask to simulate the brightness
		CanvasSolidColorBrush^ mask_color = (this->follow_global_mask_setting ? global_mask_color : this->mask_color);

		if (mask_color->Color.A != 0) {
			ds->FillRectangle(0.0F, 0.0F, width, height, mask_color);
		}
	}
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

void UniverseDisplay::on_navigate(int from_index, int to_index) {
	int delta_idx = to_index - from_index;

	if (delta_idx != 0) {
		this->from_planet = this->recent_planet;
		this->transfer(delta_idx, 0U);
	}
}

void UniverseDisplay::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->canvas->CapturePointer(args->Pointer)) {
		this->enter_critical_section();

		if (this->recent_planet != nullptr) {
			bool maniplation = ((this->figures.size() > 0) || MENUED(args->KeyModifiers));
			unsigned int id = args->Pointer->PointerId;
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			PointerUpdateKind puk = pp->Properties->PointerUpdateKind;

			this->figures.insert(std::pair<unsigned int, PointerUpdateKind>(id, puk));

			if (maniplation) {
				this->figure_x0 = pp->Position.X;
				args->Handled = true;
			} else {
				this->figure_x0 = std::nanf("swipe");
				args->Handled = this->recent_planet->on_pointer_pressed(pp->Position.X, pp->Position.Y, pdt, puk);
			}
		}

		this->leave_critical_section();
	}
}

void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;
		
		if (this->figure_x0 >= 0.0F) {
			this->figure_x = pp->Position.X;
			args->Handled = true;
		} else {
			args->Handled = this->recent_planet->on_pointer_moved(
				pp->Position.X, pp->Position.Y,
				pdt, pp->Properties->PointerUpdateKind);
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	auto it = this->figures.find(args->Pointer->PointerId);
	
	if (it != this->figures.end()) {
		this->canvas->ReleasePointerCapture(args->Pointer); // TODO: deal with PointerCaptureLost event;

		if (this->recent_planet != nullptr) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;

			if (std::isnan(this->figure_x0)) {
				this->enter_critical_section();
				args->Handled = this->recent_planet->on_pointer_released(pp->Position.X, pp->Position.Y, pdt, it->second);
				this->leave_critical_section();
			} else {
				if ((this->figures.size() == 2) || MENUED(args->KeyModifiers)) {
					this->on_translating_x();
					this->figure_x0 = std::nanf("swipe");
				}

				args->Handled = true;
			}
		}

		this->figures.erase(it);
	}
}

void UniverseDisplay::on_pointer_moveout(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	unsigned int id = args->Pointer->PointerId;
	auto it = this->figures.find(id);

	this->enter_critical_section();

	if (it != this->figures.end()) {
		this->figures.erase(it);
	}

	if (this->recent_planet != nullptr) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;

		if (this->figures.size() == 0) {
			args->Handled = this->recent_planet->on_pointer_moveout(
				pp->Position.X, pp->Position.Y,
				pdt, pp->Properties->PointerUpdateKind);
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_key(Platform::Object^ sender, KeyRoutedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		args->Handled = this->recent_planet->on_key(args->Key, false);
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_character(CoreWindow^ sender, CharacterReceivedEventArgs^ args) {
	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		unsigned int keycode = args->KeyCode;

		this->recent_planet->on_character(keycode);

		if (this->shortcuts_enabled) { // take temporary snapshot
			switch (keycode) {
			case 19: { // CTRL+S
				this->recent_planet->save(
					ms_apptemp_file(this->recent_planet->name(), ".png"),
					this->actual_width, this->actual_height);
			}; break;
			case 4: { // CTRL+D
				this->recent_planet->save_logo();
			}; break;
			case 12: { // CTRL+L
				this->recent_planet->save_logo(-2.0F, -2.0F);
			}; break;
			}
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::disable_predefined_shortcuts(bool yes) {
	this->shortcuts_enabled = !yes;
}

void UniverseDisplay::use_global_mask_setting(bool yes, bool* prev_state) {
	SET_BOX(prev_state, this->follow_global_mask_setting);

	this->follow_global_mask_setting = yes;
}

void UniverseDisplay::on_translating_x() {
	float width = this->actual_width;
	float delta = this->figure_x - this->figure_x0;
	float distance = width * 0.0382F;

	if (delta < -distance) {
		this->transfer_next(256);
	} else if (delta > distance) {
		this->transfer_previous(256);
	}
}
