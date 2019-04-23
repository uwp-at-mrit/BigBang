#include <ppltasks.h>

#include "universe.hxx"
#include "planet.hpp"
#include "navigator/null.hpp"

#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/box.hpp"
#include "datum/flonum.hpp"

#include "system.hpp"
#include "syslog.hpp"

#include "paint.hpp"
#include "brushes.hxx"
#include "colorspace.hpp"
#include "transformation.hpp"

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

static void draw_planet(CanvasDrawingSession^ ds, Platform::String^ type, IPlanet* planet, float width, float height, Syslog* logger) {
	planet->enter_shared_section();
	
	try {
		planet->draw(ds, width, height);
	} catch (Platform::Exception^ wte) {
		logger->log_message(Log::Warning, L"%s[%s]: rendering: %s", type->Data(), planet->name()->Data(), wte->Message->Data());
	}

	planet->leave_shared_section();
}

static inline float display_contain_mode_scale(float to_width, float to_height, float from_width, float from_height) {
	return flmin(flmin(to_width / from_width, to_height / from_height), 1.0F);
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
	, target_width(flmax(dest_width, 0.0F)), target_height(flmax(dest_height, 0.0F))
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

Point IDisplay::global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff, float yoff) {
	return Point(global_x + xoff, global_y + yoff);
}

Point IDisplay::local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff, float yoff) {
	return Point(local_x + xoff, local_y + yoff);
}

float IDisplay::planet_actual_width(IPlanet* p) {
	return this->actual_width;
}

float IDisplay::planet_actual_height(IPlanet* p) {
	return this->actual_height;
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

void IDisplay::save(Platform::String^ path, float dpi) {
	CanvasRenderTarget^ snapshot = this->take_snapshot(dpi);

	if (path_only(path) == nullptr) {
		CreationCollisionOption oie = CreationCollisionOption::OpenIfExists;
		CreationCollisionOption re = CreationCollisionOption::ReplaceExisting;
		Platform::String^ root = Package::Current->DisplayName;

		/** WARNING: Stupid Windows 10
		 * Saving through `IRandomAccessStream` is the only working way,
		 * and the `CanvasBitmapFileFormat::Auto` option is lost.
		 */

		create_task(KnownFolders::PicturesLibrary->CreateFolderAsync(root, oie)).then([=](task<StorageFolder^> getting) {
			return create_task(getting.get()->CreateFileAsync(path, re)).then([=](task<StorageFile^> creating) {
				return create_task(creating.get()->OpenAsync(FileAccessMode::ReadWrite)).then([=](task<IRandomAccessStream^> opening) {
					return create_task(snapshot->SaveAsync(opening.get(), CanvasBitmapFileFormat::Png, 1.0F));
				});
			});
		}).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice, L"universe[%s] has been saved to [My Picture]\\%s\\%s",
					this->get_logger()->get_name()->Data(), root->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic, L"failed to save universe[%s] to [My Pictures]\\%s\\%s: %s",
					this->get_logger()->get_name()->Data(), root->Data(), path->Data(), e->Message->Data());
			}
		});
	} else {
		create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice, L"universe[%s] has been saved to %s",
					this->get_logger()->get_name()->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic, L"failed to save universe[%s] to %s: %s",
					this->get_logger()->get_name()->Data(), path->Data(), e->Message->Data());
			}
		});
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
	: IDisplay(((logger == nullptr) ? make_silent_logger("UniverseDisplay") : logger), mode, dwidth, dheight, swidth, sheight)
	, figure_x0(flnan_f), shortcuts_enabled(true), universe_settings(nullptr), follow_global_mask_setting(true)
	, hup_top_margin(0.0F), hup_right_margin(0.0F), hup_bottom_margin(0.0F), hup_left_margin(0.0F) {
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

	if (heads_up_planet != nullptr) {
		PlanetInfo* info = bind_planet_owership(this, heads_up_planet);

		info->next = heads_up_planet;
		info->prev = heads_up_planet;

		this->headup_planet = heads_up_planet;
		this->headup_planet->fill_margin(&this->hup_top_margin, &this->hup_right_margin, &this->hup_bottom_margin, &this->hup_left_margin);
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
	if ((this->headup_planet == which) || (this->recent_planet == which)) {
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
				this->universe_settings->Values->Insert(page_setting_key, this->recent_planet->name());
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
					PlanetInfo* info = PLANET_INFO(child);

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
			PlanetInfo* info = PLANET_INFO(child);

			construct_planet(child, "planet", this->get_logger(), args->Reason, width, height);
			child = info->next;
		} while (child != this->head_planet);


		if (this->universe_settings != nullptr) {
			this->transfer_to(this->universe_settings->Values->Lookup(page_setting_key)->ToString());
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
			if ((width == region.Width) && (height == region.Height)) {
				draw_planet(ds, "planet", this->recent_planet, width, height, this->get_logger());
			} else {
				float3x2 identity = ds->Transform;

				ds->Transform = make_translation_matrix(this->hup_left_margin, this->hup_top_margin);
				draw_planet(ds, "planet", this->recent_planet, width, height, this->get_logger());
				ds->Transform = identity;
			}
		} else {
			float deltaX = ((this->transferX < 0.0F) ? width : -width);
			float tx = this->transferX + this->hup_left_margin;
			float ty = this->hup_top_margin;
			float3x2 identity = ds->Transform;

			ds->Transform = make_translation_matrix(tx, ty);
			draw_planet(ds, "planet", this->from_planet, width, height, this->get_logger());

			ds->Transform = make_translation_matrix(tx + deltaX, ty);
			draw_planet(ds, "planet", this->recent_planet, width, height, this->get_logger());

			ds->Transform = identity;
		}
	}

	if (this->headup_planet != nullptr) {
		draw_planet(ds, "heads-up", this->headup_planet, region.Width, region.Height, this->get_logger());
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
			bool maniplation = ((this->figures.size() > 0) || MENUED(args->KeyModifiers));
			unsigned int id = args->Pointer->PointerId;
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			PointerUpdateKind puk = pp->Properties->PointerUpdateKind;
			float px = pp->Position.X - this->hup_left_margin;
			float py = pp->Position.Y - this->hup_top_margin;

			this->figures.insert(std::pair<unsigned int, PointerUpdateKind>(id, puk));

			if (maniplation) {
				this->figure_x0 = px;
				args->Handled = true;
			} else {
				bool handled = false;

				this->figure_x0 = flnan_f;

				if (this->headup_planet != nullptr) {
					handled = this->headup_planet->on_pointer_pressed(pp->Position.X, pp->Position.Y, pdt, puk);
				}

				if ((!handled) && (this->recent_planet != nullptr)) {
					handled = this->recent_planet->on_pointer_pressed(px, py, pdt, puk);
				}

				args->Handled = handled;
			}
		}

		this->leave_critical_section();
	}
}

void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	this->enter_critical_section();

	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
		PointerDeviceType pdt = args->Pointer->PointerDeviceType;
		float px = pp->Position.X - this->hup_left_margin;
		float py = pp->Position.Y - this->hup_top_margin;
		
		if (this->figure_x0 >= 0.0F) {
			this->figure_x = px;
			args->Handled = true;
		} else {
			bool handled = false;

			if (this->headup_planet != nullptr) {
				handled = this->headup_planet->on_pointer_moved(pp->Position.X, pp->Position.Y, pdt, pp->Properties->PointerUpdateKind);
			}

			if ((!handled) && (this->recent_planet != nullptr)) {
				handled = this->recent_planet->on_pointer_moved(px, py, pdt, pp->Properties->PointerUpdateKind);
			}

			args->Handled = handled;
		}
	}

	this->leave_critical_section();
}

void UniverseDisplay::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	auto it = this->figures.find(args->Pointer->PointerId);
	
	if (it != this->figures.end()) {
		this->canvas->ReleasePointerCapture(args->Pointer); // TODO: deal with PointerCaptureLost event;

		if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
			PointerPoint^ pp = args->GetCurrentPoint(this->canvas);
			PointerDeviceType pdt = args->Pointer->PointerDeviceType;
			float px = pp->Position.X - this->hup_left_margin;
			float py = pp->Position.Y - this->hup_top_margin;

			if (std::isnan(this->figure_x0)) {
				bool handled = false;

				this->enter_critical_section();

				if (this->headup_planet != nullptr) {
					handled = this->headup_planet->on_pointer_released(pp->Position.X, pp->Position.Y, pdt, it->second);
				}

				if ((!handled) && (this->recent_planet != nullptr)) {
					handled = this->recent_planet->on_pointer_released(px, py, pdt, it->second);
				}

				this->leave_critical_section();

				args->Handled = handled;
			} else {
				if ((this->figures.size() == 3) || MENUED(args->KeyModifiers)) {
					this->on_translating_x();
					this->figure_x0 = flnan_f;
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

void UniverseDisplay::on_key(Platform::Object^ sender, KeyRoutedEventArgs^ args) {
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
}

void UniverseDisplay::on_character(CoreWindow^ sender, CharacterReceivedEventArgs^ args) {
	this->enter_critical_section();

	if ((this->headup_planet != nullptr) || (this->recent_planet != nullptr)) {
		unsigned int keycode = args->KeyCode;
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

void UniverseDisplay::notify_transfer(IPlanet* from, IPlanet* to) {
	if (this->headup_planet != nullptr) {
		this->headup_planet->on_transfer(from, to);
	}

	if (this->head_planet != nullptr) {
		IPlanet* child = this->head_planet;

		do {
			PlanetInfo* info = PLANET_INFO(child);

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
		float3x2 identity = ds->Transform;

		ds->Transform = make_translation_matrix(this->hup_left_margin, this->hup_top_margin);
		draw_planet(ds, "planet", this->recent_planet, width, height, this->get_logger());
		ds->Transform = identity;
	}

	if (this->headup_planet != nullptr) {
		draw_planet(ds, "heads-up", this->headup_planet, region.Width, region.Height, this->get_logger());
	}

	return snapshot;
}
