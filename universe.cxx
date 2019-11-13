#include <ppltasks.h>

#include "universe.hxx"
#include "planet.hpp"

#include "navigator/null.hpp"
#include "virtualization/screen/pasteboard.hpp"

#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/box.hpp"
#include "datum/flonum.hpp"

#include "preference.hxx"
#include "system.hpp"
#include "syslog.hpp"

#include "math.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Windows::System;
using namespace Windows::Devices::Input;
using namespace Windows::ApplicationModel;

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

#define PLANET_INFO(planet) (static_cast<LinkedPlanetInfo*>(planet->info))

#define SHIFTED(vkms) ((vkms & VirtualKeyModifiers::Shift) == VirtualKeyModifiers::Shift)
#define CONTROLLED(vkms) ((vkms & VirtualKeyModifiers::Control) == VirtualKeyModifiers::Control)
#define WINDOWED(vkms) ((vkms & VirtualKeyModifiers::Windows) == VirtualKeyModifiers::Windows)
#define MENUED(vkms) ((vkms & VirtualKeyModifiers::Menu) == VirtualKeyModifiers::Menu)

static Platform::String^ page_setting_key = "Page_Name";
static Platform::String^ mask_setting_key = "Mask_Alpha";

static CanvasSolidColorBrush^ global_mask_color;

namespace {
	private class LinkedPlanetInfo : public WarGrey::SCADA::IPlanetInfo {
	public:
		LinkedPlanetInfo(IScreen* master) : IPlanetInfo(master) {};

	public:
		IPlanet* next;
		IPlanet* prev;
	};
}

private struct WarGrey::SCADA::UniverseFigure {
public:
	UniverseFigure(unsigned int seq, PointerUpdateKind kind, float x, float y) : seq(seq), kind(kind) {
		this->push_point(x, y);
	}

public:
	void push_point(float x, float y) {
		this->points.push_back(float2(x, y));
	}

public:
	unsigned int seq;
	PointerUpdateKind kind;

public:
	std::deque<float2> points;
};

static GraphletGesture gesture_recognize(UniverseFigure* first, UniverseFigure* second, float* param1, float* param2, float* param3) {
	GraphletGesture gesture = GraphletGesture::_;
	size_t maxidx1 = first->points.size() - 1;
	size_t maxidx2 = second->points.size() - 1;
	size_t lstidx1 = maxidx1 - 1;
	size_t lstidx2 = maxidx2 - 1;

	if ((maxidx1 >= 2) && (maxidx2 >= 2)) {
		float2 pt10 = first->points[0];
		float2 pt11 = first->points[maxidx1];
		float2 pt1l = first->points[lstidx1];
		float2 pt20 = second->points[0];
		float2 pt21 = second->points[maxidx2];
		float2 pt2l = second->points[lstidx2];
		float vx1 = pt11.x - pt10.x;
		float vy1 = pt11.y - pt10.y;
		float vx2 = pt21.x - pt20.x;
		float vy2 = pt21.y - pt20.y;

		if (dot_product(vx1, vy1, vx2, vy2) > 0.0F) { // same direction, for translation
			gesture = GraphletGesture::Translation;
			SET_BOX(param1, flmin(pt11.x - pt1l.x, pt21.x - pt2l.x));
			SET_BOX(param2, flmin(pt11.y - pt1l.y, pt21.y - pt2l.y));
		} else { // different direction, for scale
			float distance0 = points_distance_squared(pt10.x, pt10.y, pt20.x, pt20.y);
			float distance1 = points_distance_squared(pt11.x, pt11.y, pt21.x, pt21.y);
			
			gesture = GraphletGesture::Zoom;
			line_point(pt10, pt20, 0.5, param1, param2);
			SET_BOX(param3, (distance1 - distance0) * 0.5F);
		}
	}

	return gesture;
}

/*************************************************************************************************/
static inline LinkedPlanetInfo* bind_planet_owership(IScreen* master, IPlanet* planet) {
	auto info = new LinkedPlanetInfo(master);
	
	planet->info = info;

	return info;
}

static void construct_planet(IPlanet* planet, Platform::String^ type, Syslog* logger, CanvasCreateResourcesReason reason, float width, float height) {
	planet->begin_update_sequence();

	try {
		planet->construct(reason, width, height);
		planet->load(reason, width, height);
		planet->reflow(width, height);
		planet->notify_surface_ready();

		logger->log_message(Log::Debug, L"%s[%s] is constructed in region[%f, %f]", type->Data(), planet->name()->Data(), width, height);
	} catch (Platform::Exception^ e) {
		logger->log_message(Log::Critical, L"%s: constructing: %s", planet->name()->Data(), e->Message->Data());
	}

	planet->end_update_sequence();
}

static inline void reflow_planet(IPlanet* planet, float width, float height) {
	if (planet->surface_ready()) {
		planet->enter_critical_section();
		planet->reflow(width, height);
		planet->leave_critical_section();
	}
}

static void draw_planet(CanvasDrawingSession^ ds, Platform::String^ type, IPlanet* planet, float x, float y, float width, float height, Syslog* logger) {
	planet->enter_shared_section();
	
	try {
		planet->draw(ds, x, y, width, height);
	} catch (Platform::Exception^ wte) {
		logger->log_message(Log::Warning, L"%s[%s]: rendering: %s", type->Data(), planet->name()->Data(), wte->Message->Data());
	}

	planet->leave_shared_section();
}

static inline void set_mask_alpha(CanvasSolidColorBrush^ color, double v) {
	color->Color = rgba(color->Color, v);
}

static inline double get_mask_alpha(CanvasSolidColorBrush^ color) {
	return double(color->Color.A) / 255.0;
}

static inline void load_mask_alpha(ApplicationDataContainer^ zone, CanvasSolidColorBrush^ color) {
	if (has_preference(mask_setting_key, zone)) {
		set_mask_alpha(color, get_preference(mask_setting_key, 0.0, zone));
	}
}

/*************************************************************************************************/
UniverseDisplay::UniverseDisplay(Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IHeadUpPlanet* heads_up_planet)
	: UniverseDisplay(DisplayFit::None, 0.0F, 0.0F, logger, setting_name, navigator, heads_up_planet) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight
	, Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IHeadUpPlanet* heads_up_planet)
	: UniverseDisplay(mode, dwidth, dheight, dwidth, dheight, logger, setting_name, navigator, heads_up_planet) {}

UniverseDisplay::UniverseDisplay(DisplayFit mode, float dwidth, float dheight, float swidth, float sheight
	, Syslog* logger, Platform::String^ setting_name, IUniverseNavigator* navigator, IHeadUpPlanet* heads_up_planet)
	: IDisplay(((logger == nullptr) ? make_silent_logger("UniverseDisplay") : logger))
	, shortcuts_enabled(true), universe_settings(nullptr), follow_global_mask_setting(true)
	, hup_top_margin(0.0F), hup_right_margin(0.0F), hup_bottom_margin(0.0F), hup_left_margin(0.0F) {
	this->transfer_clock = ref new DispatcherTimer();
	this->transfer_clock->Tick += ref new EventHandler<Platform::Object^>(this, &UniverseDisplay::do_refresh);

	this->_navigator = ((navigator == nullptr) ? new NullNavigator() : navigator);
	this->_navigator->push_navigation_listener(this);

	if (setting_name != nullptr) {
		this->universe_settings = make_preference_zone(setting_name);
	}

	this->display = ref new CanvasControl();
	this->display->Name = this->get_logger()->get_name();
	this->screen = new Pasteboard(this, mode, dwidth, dheight, swidth, sheight);

	if (heads_up_planet != nullptr) {
		LinkedPlanetInfo* info = bind_planet_owership(this->screen, heads_up_planet);

		info->next = heads_up_planet;
		info->prev = heads_up_planet;

		this->headup_planet = heads_up_planet;
		this->headup_planet->fill_margin(&this->hup_top_margin, &this->hup_right_margin, &this->hup_bottom_margin, &this->hup_left_margin);
	}

	{ // initialize gesture
		auto master = CoreWindow::GetForCurrentThread();
	    
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
		this->display->PointerWheelChanged += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_wheel);

		master->CharacterReceived += ref new TypedEventHandler<CoreWindow^, CharacterReceivedEventArgs^>(this, &UniverseDisplay::on_keycode);
	}

	{ // initialize masks
		this->mask_color = make_solid_brush(0x000000, 0.0F);

		if (global_mask_color == nullptr) {
			global_mask_color = make_solid_brush(0x000000, 0.0F);
		}

		load_mask_alpha(nullptr, global_mask_color);

		if (this->universe_settings != nullptr) {
			load_mask_alpha(this->universe_settings, this->mask_color);
		}
	}
}

void UniverseDisplay::register_virtual_keydown_event_handler(UIElement^ target) {
	target->KeyDown += ref new KeyEventHandler(this, &UniverseDisplay::on_virtual_key);
}

UniverseDisplay::~UniverseDisplay() {
	this->transfer_clock->Stop();
	this->collapse();
	
	if (this->screen != nullptr) {
		delete this->screen;
	}

	if (this->headup_planet != nullptr) {
		delete this->headup_planet;
	}

	if (this->_navigator != nullptr) {
		delete this->_navigator;
	}
}

IUniverseNavigator* UniverseDisplay::navigator::get() {
	return this->_navigator;
}

IHeadUpPlanet* UniverseDisplay::heads_up_planet::get() {
	return this->headup_planet;
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
	put_preference(mask_setting_key, v);
	set_mask_alpha(global_mask_color, v);

	this->refresh(this->recent_planet);
}

double UniverseDisplay::global_mask_alpha::get() {
	return get_mask_alpha(global_mask_color);
}

void UniverseDisplay::mask_alpha::set(double v) {
	set_mask_alpha(this->mask_color, v);

	if (this->universe_settings != nullptr) {
		put_preference(mask_setting_key, v, this->universe_settings);
	}

	this->refresh(this->recent_planet);
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
	if ((which == nullptr) || (this->headup_planet == which) || (this->recent_planet == which)) {
		this->display->Invalidate();
	}
}

void UniverseDisplay::on_elapse(long long count, long long interval, long long uptime) {
	if (this->headup_planet != nullptr) {
		this->headup_planet->begin_update_sequence();
		this->headup_planet->on_elapse(count, interval, uptime);
		this->headup_planet->end_update_sequence();
	}

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
	if (this->headup_planet != nullptr) {
		this->headup_planet->begin_update_sequence();
		this->headup_planet->on_elapse(count, interval, uptime, elapsed);
		this->headup_planet->end_update_sequence();
	}

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
		LinkedPlanetInfo* info = bind_planet_owership(this->screen, planet);
		
		if (this->head_planet == nullptr) {
			this->head_planet = planet;
			this->recent_planet = planet;
			info->prev = this->head_planet;
			
			this->_navigator->insert(planet);
			this->_navigator->select(planet);

			this->get_logger()->log_message(Log::Debug, L"found the first planet[%s]", planet->name()->Data());
		} else { 
			LinkedPlanetInfo* head_info = PLANET_INFO(this->head_planet);
			LinkedPlanetInfo* prev_info = PLANET_INFO(head_info->prev);
			 
			info->prev = head_info->prev;
			prev_info->next = planet;
			head_info->prev = planet;

			this->_navigator->insert(planet);

			this->get_logger()->log_message(Log::Debug, L"found another planet[%s]", planet->name()->Data());
		}

		info->next = this->head_planet;

		if (this->universe_settings != nullptr) {
			if (!has_preference(page_setting_key, this->universe_settings)) {
				put_preference(page_setting_key, planet->name(), this->universe_settings);
			}
		}
	}
}

void UniverseDisplay::transfer(int delta_idx, unsigned int ms, unsigned int count) {
	if ((this->recent_planet != nullptr) && (delta_idx != 0) && (!this->transfer_clock->IsEnabled)) {
		bool animating = ((ms * count) > 0);

		if (this->from_planet == nullptr) {
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

		{ // trigger point
			this->_navigator->select(this->recent_planet);

			if (this->universe_settings != nullptr) {
				put_preference(page_setting_key, this->recent_planet->name(), this->universe_settings);
			}

			this->notify_transfer(this->from_planet, this->recent_planet);
		}

		if (animating) {
			TimeSpan ts = make_timespan_from_milliseconds(ms);
			float width = this->display->Size.Width - this->hup_left_margin - this->hup_right_margin;

			ts.Duration = ms / count;
			this->transfer_clock->Interval = ts;
			this->transfer_delta = width / float(count) * ((delta_idx > 0) ? -1.0F : 1.0F);
			this->transferX = this->transfer_delta;
			this->transfer_clock->Start();
		} else {
			this->get_logger()->log_message(Log::Debug, L"transferred immediately: %s ==> %s",
				this->from_planet->name()->Data(), this->recent_planet->name()->Data());
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
			LinkedPlanetInfo* info = PLANET_INFO(child);

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
		this->transfer(delta_idx, ms, count);
	}
}

void UniverseDisplay::transfer_previous(unsigned int ms, unsigned int count) {
	this->transfer(-1, ms, count);
}

void UniverseDisplay::transfer_next(unsigned int ms, unsigned int count) {
	this->transfer(1, ms, count);
}

void UniverseDisplay::on_navigate(int from_index, int to_index) {
	int delta_idx = to_index - from_index;

	if (delta_idx != 0) {
		this->from_planet = this->recent_planet;
		this->transfer(delta_idx, 0U);
	}
}

void UniverseDisplay::collapse() {
	if (this->head_planet != nullptr) {
		IPlanet* temp_head = this->head_planet;
		LinkedPlanetInfo* temp_info = PLANET_INFO(temp_head);
		LinkedPlanetInfo* prev_info = PLANET_INFO(temp_info->prev);

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
	if ((this->headup_planet != nullptr) || (this->head_planet != nullptr)) {
		float nwidth = args->NewSize.Width;
		float nheight = args->NewSize.Height;
		float pwidth = args->PreviousSize.Width;
		float pheight = args->PreviousSize.Height;

		if ((nwidth > 0.0F) && (nheight > 0.0F) && ((nwidth != pwidth) || (nheight != pheight))) {
			this->get_logger()->log_message(Log::Debug, L"resize(%f, %f)", nwidth, nheight);

			if (this->headup_planet != nullptr) {
				reflow_planet(this->headup_planet, nwidth, nheight);
			}

			if (this->head_planet != nullptr) {
				IPlanet* child = this->head_planet;
				float width = nwidth - this->hup_left_margin - this->hup_right_margin;
				float height = nheight - this->hup_top_margin - this->hup_bottom_margin;

				do {
					LinkedPlanetInfo* info = PLANET_INFO(child);

					reflow_planet(child, width, height);
					child = info->next;
				} while (child != this->head_planet);
			}
		}
	}
}

void UniverseDisplay::do_construct(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	Size region = this->display->Size;
	
	this->get_logger()->log_message(Log::Debug, L"construct planets because of %s", args->Reason.ToString()->Data());

	if (this->headup_planet != nullptr) {
		construct_planet(this->headup_planet, "heads-up", this->get_logger(), args->Reason, region.Width, region.Height);
	}

	this->construct(args->Reason);

	if (this->head_planet != nullptr) {
		IPlanet* child = this->head_planet;
		Platform::String^ last_page = nullptr;
		float width = region.Width - this->hup_left_margin - this->hup_right_margin;
		float height = region.Height - this->hup_top_margin - this->hup_bottom_margin;

		do {
			LinkedPlanetInfo* info = PLANET_INFO(child);

			construct_planet(child, "planet", this->get_logger(), args->Reason, width, height);
			child = info->next;
		} while (child != this->head_planet);


		if (this->universe_settings != nullptr) {
			this->transfer_to(get_preference(page_setting_key, this->universe_settings)->ToString());
		}

		if ((this->recent_planet == this->head_planet) && (this->recent_planet != nullptr)) {
			this->notify_transfer(nullptr, this->recent_planet);
		}
	}
}

void UniverseDisplay::do_paint(CanvasControl^ sender, CanvasDrawEventArgs^ args) {
	CanvasDrawingSession^ ds = args->DrawingSession;
	Size region = this->display->Size;

	// NOTE: only the heads-up planet, current planet and the one transferred from need to be drawn

	this->enter_critical_section();

	if (this->recent_planet != nullptr) {
		float width = region.Width - this->hup_left_margin - this->hup_right_margin;
		float height = region.Height - this->hup_top_margin - this->hup_bottom_margin;

		if (this->from_planet == nullptr) {
			draw_planet(ds, "planet", this->recent_planet, this->hup_left_margin, this->hup_top_margin, width, height, this->get_logger());
		} else {
			float deltaX = ((this->transferX < 0.0F) ? width : -width);
			float tx = this->transferX + this->hup_left_margin;
			float ty = this->hup_top_margin;
			
			draw_planet(ds, "planet", this->from_planet, tx, ty, width, height, this->get_logger());
			draw_planet(ds, "planet", this->recent_planet, tx + deltaX, ty, width, height, this->get_logger());
		}
	}

	if (this->headup_planet != nullptr) {
		draw_planet(ds, "heads-up", this->headup_planet, 0.0F, 0.0F, region.Width, region.Height, this->get_logger());
	}

	this->leave_critical_section();
	
	{ // draw mask to simulate the brightness
		CanvasSolidColorBrush^ mask_color = (this->follow_global_mask_setting ? global_mask_color : this->mask_color);

		if (mask_color->Color.A != 0) {
			ds->FillRectangle(0.0F, 0.0F, region.Width, region.Height, mask_color);
		}
	}
}

void UniverseDisplay::do_refresh(Platform::Object^ sender, Platform::Object^ args) {
	const wchar_t* from = this->from_planet->name()->Data();
	const wchar_t* to = this->recent_planet->name()->Data();
	float width = this->display->Size.Width - this->hup_left_margin - this->hup_right_margin;
	float percentage = flabs(this->transferX) / width;

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

Point UniverseDisplay::global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff, float yoff) {
	if (this->headup_planet != p) {
		xoff -= this->hup_left_margin;
		yoff -= this->hup_top_margin;
	}

	return Point(global_x + xoff, global_y + yoff);
}

Point UniverseDisplay::local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff, float yoff) {
	if (this->headup_planet != p) {
		xoff += this->hup_left_margin;
		yoff += this->hup_top_margin;
	}

	return Point(local_x + xoff, local_y + yoff);
}

float UniverseDisplay::planet_actual_width(IPlanet* p) {
	return this->actual_width
		- ((this->headup_planet != p)
			? (this->hup_left_margin + this->hup_right_margin)
			: 0.0F);
}

float UniverseDisplay::planet_actual_height(IPlanet* p) {
	return this->actual_height
		- ((this->headup_planet != p)
			? (this->hup_top_margin + this->hup_bottom_margin)
			: 0.0F);
}

void UniverseDisplay::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->canvas->CapturePointer(args->Pointer)) {
		this->enter_critical_section();

		if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
			unsigned int id = args->Pointer->PointerId;
			size_t sequence_id = this->figures.size() + 1;
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			PointerUpdateKind puk = pp->Properties->PointerUpdateKind;
			float px = pp->Position.X - this->hup_left_margin;
			float py = pp->Position.Y - this->hup_top_margin;

			this->figures.insert(std::pair<unsigned int, UniverseFigure>(id, UniverseFigure((unsigned int)sequence_id, puk, px, py)));

			if (sequence_id == 1) {
				bool handled = false;

				if (this->headup_planet != nullptr) {
					handled = this->headup_planet->on_pointer_pressed(pp->Position.X, pp->Position.Y, pdt, puk);
				}

				if ((!handled) && (this->recent_planet != nullptr)) {
					handled = this->recent_planet->on_pointer_pressed(px, py, pdt, puk);
				}

				args->Handled = handled;
			} else {
				region_fuse_reset(&this->gesture_lt, &this->gesture_rb);
				region_fuse_point(&this->gesture_lt, &this->gesture_rb, px, py);

				args->Handled = true;
			}
		}

		this->leave_critical_section();
	}
}

void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	unsigned int id = args->Pointer->PointerId;
	auto it = this->figures.find(id);
	bool handled = false;

	if (it != this->figures.end()) {
		if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			float px = pp->Position.X - this->hup_left_margin;
			float py = pp->Position.Y - this->hup_top_margin;
			size_t size = this->figures.size();

			if (size == 1) {
				this->enter_critical_section();

				if (this->headup_planet != nullptr) {
					handled = this->headup_planet->on_pointer_moved(pp->Position.X, pp->Position.Y, pdt, pp->Properties->PointerUpdateKind);
				}

				if ((!handled) && (this->recent_planet != nullptr)) {
					handled = this->recent_planet->on_pointer_moved(px, py, pdt, pp->Properties->PointerUpdateKind);
				}

				this->leave_critical_section();
			} else {
				it->second.push_point(px, py);
				region_fuse_point(&this->gesture_lt, &this->gesture_rb, px, py);

				if (size == 2) {
					if ((this->recent_planet != nullptr) && this->recent_planet->can_affine_transform(this->gesture_lt, this->gesture_rb)) {
						UniverseFigure* first = nullptr;
						UniverseFigure* second = nullptr;

						for (auto it = this->figures.begin(); it != this->figures.end(); it++) {
							switch (it->second.seq) {
							case 1: first = &it->second; break;
							case 2: second = &it->second; break;
							}
						}

						{ // do affine transforming
							float param1 = 0.0F;
							float param2 = 0.0F;
							float param3 = 0.0F;
							GraphletGesture gesture = gesture_recognize(first, second, &param1, &param2, &param3);

							if (gesture != GraphletGesture::_) {
								this->enter_critical_section();

								switch (gesture) {
								case GraphletGesture::Translation: this->recent_planet->on_translation_gesture(param1, param2, this->gesture_lt, this->gesture_rb); break;
								case GraphletGesture::Zoom: this->recent_planet->on_zoom_gesture(param1, param2, param3, this->gesture_lt, this->gesture_rb); break;
								}

								this->leave_critical_section();
								handled = true;
							}
						}
					}
				}
			}
		}
	}

	args->Handled = handled;
}

void UniverseDisplay::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	unsigned int id = args->Pointer->PointerId;
	auto it = this->figures.find(id);

	if (it != this->figures.end()) {
		this->canvas->ReleasePointerCapture(args->Pointer); // TODO: deal with PointerCaptureLost event;

		if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			float px = pp->Position.X - this->hup_left_margin;
			float py = pp->Position.Y - this->hup_top_margin;

			switch (this->figures.size()) {
			case 1: {
				bool handled = false;

				this->enter_critical_section();

				if (this->headup_planet != nullptr) {
					handled = this->headup_planet->on_pointer_released(pp->Position.X, pp->Position.Y, pdt, it->second.kind);
				}

				if ((!handled) && (this->recent_planet != nullptr)) {
					handled = this->recent_planet->on_pointer_released(px, py, pdt, it->second.kind);
				}

				this->leave_critical_section();

				args->Handled = handled;
			}; break;
			case 3: {
				this->on_translating_x(px - it->second.points[0].x);
				args->Handled = true;
			}; break;
			}
		}

		this->figures.clear();
	}
}

void UniverseDisplay::on_pointer_moveout(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	unsigned int id = args->Pointer->PointerId;
	auto it = this->figures.find(id);

	this->enter_critical_section();

	if (it != this->figures.end()) {
		this->figures.erase(it);
	}

	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;
		float px = pp->Position.X - this->hup_left_margin;
		float py = pp->Position.Y - this->hup_top_margin;

		if (this->figures.size() == 0) {
			bool handled = false;

			if (this->headup_planet != nullptr) {
				handled = this->headup_planet->on_pointer_moveout(pp->Position.X, pp->Position.Y, pdt, pp->Properties->PointerUpdateKind);
			} 

			if ((!handled) && (this->recent_planet != nullptr)) {
				handled = this->recent_planet->on_pointer_moveout(px, py, pdt, pp->Properties->PointerUpdateKind);
			}

			args->Handled = handled;
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_pointer_wheel(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		VirtualKeyModifiers modifies = args->KeyModifiers;
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerPointProperties^ ppt = pp->Properties;
		float delta = float(ppt->MouseWheelDelta / WHEEL_DELTA);
		bool horizontal = ppt->IsHorizontalMouseWheel;
		bool controlled = CONTROLLED(args->KeyModifiers);
		bool handled = false;

		this->enter_critical_section();

		if (this->headup_planet != nullptr) {
			handled = this->headup_planet->on_pointer_wheeled(pp->Position.X, pp->Position.Y, delta, horizontal, controlled);
		}

		if ((!handled) && (this->recent_planet != nullptr)) {
			handled = this->recent_planet->on_pointer_wheeled(pp->Position.X, pp->Position.Y, delta, horizontal, controlled);
		}

		this->leave_critical_section();

		args->Handled = handled;
	}
}

void UniverseDisplay::on_virtual_key(Platform::Object^ sender, KeyRoutedEventArgs^ args) {
	this->enter_critical_section();

	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		bool handled = false;

		if (this->headup_planet != nullptr) {
			handled = this->headup_planet->on_key(args->Key, false);
		}

		if ((!handled) && (this->recent_planet != nullptr)) {
			handled = this->recent_planet->on_key(args->Key, false);
		}

		args->Handled = handled;
	}

	this->leave_critical_section();

	if (!args->Handled) {
		args->Handled = this->on_key(args->Key, false);
	}
}

void UniverseDisplay::on_keycode(CoreWindow^ sender, CharacterReceivedEventArgs^ args) {
	unsigned int keycode = args->KeyCode;
	
	this->enter_critical_section();

	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		bool handled = false;

		if (this->headup_planet != nullptr) {
			handled = this->headup_planet->on_character(keycode);
		}

		if ((!handled) && (this->recent_planet != nullptr)) {
			handled = this->recent_planet->on_character(keycode);

			if (this->shortcuts_enabled) { // take temporary snapshot
				switch (keycode) {
				case 19: { // CTRL+S
					this->recent_planet->save(
						ms_apptemp_file(this->recent_planet->name(), ".png"),
						this->actual_width, this->actual_height);
				}; break;
				case 4: { // CTRL+D
					this->recent_planet->save_logo(-2.0F, -2.0F);
				}; break;
				case 12: { // CTRL+L
					this->recent_planet->save_logo();
				}; break;
				}
			}
		}

		args->Handled = handled;
	}

	this->leave_critical_section();

	if (!args->Handled) {
		args->Handled = this->on_character(keycode);
	}
}

void UniverseDisplay::disable_predefined_shortcuts(bool yes) {
	this->shortcuts_enabled = !yes;
}

void UniverseDisplay::use_global_mask_setting(bool yes, bool* prev_state) {
	SET_BOX(prev_state, this->follow_global_mask_setting);

	this->follow_global_mask_setting = yes;
}

void UniverseDisplay::on_translating_x(float delta) {
	float width = this->actual_width;
	float distance = width * 0.0382F;

	if (delta < -distance) {
		this->transfer_next(256);
	} else if (delta > distance) {
		this->transfer_previous(256);
	}
}

void UniverseDisplay::notify_transfer(IPlanet* from, IPlanet* to) {
	if (this->headup_planet != nullptr) {
		this->headup_planet->on_transfer(from, to);
	}

	if (this->head_planet != nullptr) {
		IPlanet* child = this->head_planet;

		do {
			LinkedPlanetInfo* info = PLANET_INFO(child);

			child->on_transfer(from, to);
			child = info->next;
		} while (child != this->head_planet);
	}
}

/*************************************************************************************************/
CanvasRenderTarget^ UniverseDisplay::take_snapshot(float dpi) {
	Size region = this->display->Size;
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	CanvasRenderTarget^ snapshot = ref new CanvasRenderTarget(shared_dc, region.Width, region.Height, dpi);
	CanvasDrawingSession^ ds = snapshot->CreateDrawingSession();

	/** NOTE
	 * Here is not a necessary critical section since planets have their own critical sections.
	 *
	 * Therefore, Does `IDisplay` really have to define so many critical sections in this file?
	 */

	ds->Clear(Colours::Background->Color);

	if (this->recent_planet != nullptr) {
		float width = region.Width - this->hup_left_margin - this->hup_right_margin;
		float height = region.Height - this->hup_top_margin - this->hup_bottom_margin;
		
		draw_planet(ds, "planet", this->recent_planet, this->hup_left_margin, this->hup_top_margin, width, height, this->get_logger());
	}

	if (this->headup_planet != nullptr) {
		draw_planet(ds, "heads-up", this->headup_planet, 0.0F, 0.0F, region.Width, region.Height, this->get_logger());
	}

	return snapshot;
}
