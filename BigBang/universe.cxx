#include "universe.hxx"
#include "planet.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

#define PLANET_INFO(planet) (static_cast<PlanetInfo*>(planet->info))

class PlanetInfo : public WarGrey::SCADA::IPlanetInfo {
public:
	PlanetInfo(IDisplay^ master) : IPlanetInfo(master) {};

public:
	bool loaded = false;
	bool pending_resize = false;

public:
	IPlanet* next;
	IPlanet* prev;
};

static inline PlanetInfo* bind_planet_owership(IDisplay^ master, IPlanet* planet) {
	auto info = new PlanetInfo(master);
	planet->info = info;

	return info;
}

/*************************************************************************************************/
float IDisplay::actual_width::get() { return float(this->canvas->ActualWidth); };
float IDisplay::actual_height::get() { return float(this->canvas->ActualHeight); };

void IDisplay::max_width::set(float v) { this->canvas->MaxWidth = double(v); }
float IDisplay::max_width::get() { return float(this->canvas->MaxWidth); };

void IDisplay::max_height::set(float v) { this->canvas->MaxHeight = double(v); }
float IDisplay::max_height::get() { return float(this->canvas->MaxHeight); };

void IDisplay::min_width::set(float v) { this->canvas->MinWidth = double(v); }
float IDisplay::min_width::get() { return float(this->canvas->MinWidth); };

void IDisplay::min_height::set(float v) { this->canvas->MinHeight = double(v); }
float IDisplay::min_height::get() { return float(this->canvas->MinHeight); };

void IDisplay::width::set(float v) { this->canvas->Width = double(v); }
float IDisplay::width::get() { return float(this->canvas->Width); };

void IDisplay::height::set(float v) { this->canvas->Height = double(v); }
float IDisplay::height::get() { return float(this->canvas->Height); };

/*************************************************************************************************/
UniverseDisplay::UniverseDisplay(SplitView^ parent, int frame_rate, Platform::String^ id, Log level) : parent(parent) {
	this->logger = new Syslog(level, (id != nullptr) ? id : "Universe", default_logger());
	this->logger->reference();
	
	this->display = ref new CanvasAnimatedControl();
	if (id != nullptr) this->display->Name = id;

	if (frame_rate > 0) {
		this->display->TargetElapsedTime = TimeSpan({ 10000000LL / frame_rate });
	} else if (frame_rate < 0) {
		this->display->TargetElapsedTime = TimeSpan({ -10000000LL * frame_rate });
	}

	this->display->UseSharedDevice = true; // this is required
	this->display->SizeChanged += ref new SizeChangedEventHandler(this, &UniverseDisplay::do_resize);
	this->display->CreateResources += ref new UniverseLoadHandler(this, &UniverseDisplay::do_construct);
	this->display->GameLoopStarting += ref new UniverseHandler(this, &UniverseDisplay::do_start);
	this->display->GameLoopStopped += ref new UniverseHandler(this, &UniverseDisplay::do_stop);
	this->display->Update += ref new UniverseUpdateHandler(this, &UniverseDisplay::do_update);
	this->display->Draw += ref new UniverseDrawHandler(this, &UniverseDisplay::do_paint);

	this->display->PointerMoved += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moved);
	this->display->PointerPressed += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_pressed);
	this->display->PointerReleased += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_released);

	this->parent->Content = this->display;
}

UniverseDisplay::~UniverseDisplay() {
	this->logger->destroy();
	this->collapse();
}

UserControl^ UniverseDisplay::canvas::get() { return this->display; };
CanvasDevice^ UniverseDisplay::device::get() { return this->display->Device; };

float UniverseDisplay::actual_width::get() { return float(this->display->Size.Width); };
float UniverseDisplay::actual_height::get() { return float(this->display->Size.Height); };

void UniverseDisplay::add_planet(IPlanet* planet) {
	if (planet->info == nullptr) {
		auto info = bind_planet_owership(this, planet);

		if (this->head_planet == nullptr) {
			this->head_planet = planet;
			info->prev = this->head_planet;
		} else {
			PlanetInfo* head_info = PLANET_INFO(this->head_planet);
			PlanetInfo* prev_info = PLANET_INFO(head_info->prev);

			info->prev = head_info->prev;
			prev_info->next = planet;
			head_info->prev = planet;
		}
		info->next = this->head_planet;
	}
}

void UniverseDisplay::collapse() {
	if (this->head_planet != nullptr) {
		IPlanet* temp_head = this->head_planet;
		PlanetInfo* temp_info = PLANET_INFO(temp_head);
		PlanetInfo* prev_info = PLANET_INFO(temp_info->prev);

		this->head_planet = nullptr;
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
			this->logger->log_message(Log::Info, L"resize(%f, %f)", nwidth, nheight);
			this->head_planet->reflow(nwidth, nheight);
		}
	}
}

void UniverseDisplay::do_start(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	this->logger->log_message(Log::Debug, "big bang");
	this->big_bang();
}

void UniverseDisplay::do_construct(CanvasAnimatedControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	this->logger->log_message(Log::Debug, L"construct planet because of %s", args->Reason.ToString()->Data());
	
	this->construct();
	if (this->head_planet != nullptr) {
		Size region = this->display->Size;

		this->head_planet->construct(args->Reason, region.Width, region.Height);
		this->head_planet->reflow(region.Width, region.Height);
	}
}

void UniverseDisplay::do_update(ICanvasAnimatedControl^ sender, CanvasAnimatedUpdateEventArgs^ args) {
	if (this->head_planet != nullptr) {
		long long count = args->Timing.UpdateCount - 1;
		long long elapsed = args->Timing.ElapsedTime.Duration;
		long long uptime = args->Timing.TotalTime.Duration;
		bool is_slow = args->Timing.IsRunningSlowly;

		this->head_planet->update(count, elapsed, uptime, is_slow);

		if (is_slow) {
			this->logger->log_message(Log::Notice, L"cannot update the universe within %fms.", float(elapsed) / 10000.0F);
		}
	}
}

void UniverseDisplay::do_paint(ICanvasAnimatedControl^ sender, CanvasAnimatedDrawEventArgs^ args) {
	if (this->head_planet != nullptr) {
		Size region = this->display->Size;

		try {
			this->head_planet->enter_critical_section();
			this->head_planet->draw(args->DrawingSession, region.Width, region.Height);
			this->head_planet->leave_critical_section();
		} catch (Platform::Exception^ wte) {
			this->head_planet->leave_critical_section();
			this->logger->log_message(Log::Warning, L"rendering: %s", wte->Message->Data());
		}
	}
}

void UniverseDisplay::do_stop(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	this->logger->log_message(Log::Debug, "big rip");
	this->big_rip();
}

void UniverseDisplay::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->head_planet != nullptr) {
		this->head_planet->on_pointer_moved(this->canvas, args);
	}
}

void UniverseDisplay::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->head_planet != nullptr) {
		this->head_planet->on_pointer_pressed(this->canvas, args);
	}
}

void UniverseDisplay::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	if (this->head_planet != nullptr) {
		this->head_planet->on_pointer_released(this->canvas, args);
	}
}
