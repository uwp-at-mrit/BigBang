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
UniverseDisplay::UniverseDisplay(SplitView^ parent, IPlanet* planet, int frame_rate, Platform::String^ id) : parent(parent) {
	this->display = ref new CanvasAnimatedControl();
	if (id != nullptr) this->display->Name = id;

	if (frame_rate > 0) {
		this->display->TargetElapsedTime = TimeSpan({ 10000000LL / frame_rate });
	} else if (frame_rate < 0) {
		this->display->TargetElapsedTime = TimeSpan({ -10000000LL * frame_rate });
	}

	this->display->UseSharedDevice = true; // this is required
	this->display->SizeChanged += ref new SizeChangedEventHandler(this, &UniverseDisplay::do_resize);
	this->display->CreateResources += ref new UniverseLoadHandler(this, &UniverseDisplay::do_load);
	this->display->GameLoopStarting += ref new UniverseHandler(this, &UniverseDisplay::do_start);
	this->display->GameLoopStopped += ref new UniverseHandler(this, &UniverseDisplay::do_stop);
	this->display->Update += ref new UniverseUpdateHandler(this, &UniverseDisplay::do_update);
	this->display->Draw += ref new UniverseDrawHandler(this, &UniverseDisplay::do_paint);

	this->display->PointerMoved += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_moved);
	this->display->PointerPressed += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_pressed);
	this->display->PointerReleased += ref new PointerEventHandler(this, &UniverseDisplay::on_pointer_released);

	this->head_planet = planet;
	this->head_planet->master = this;
	this->parent->Content = this->display;
}

UserControl^ UniverseDisplay::canvas::get() { return this->display; };
CanvasDevice^ UniverseDisplay::device::get() { return this->display->Device; };

float UniverseDisplay::actual_width::get() { return float(this->display->Size.Width); };
float UniverseDisplay::actual_height::get() { return float(this->display->Size.Height); };

void UniverseDisplay::do_resize(Platform::Object^ sender, SizeChangedEventArgs^ args) {
	float width = args->NewSize.Width;
	float height = args->NewSize.Height;

	if ((width > 0.0F) && (height > 0.0F)) {
		if (this->loaded) {
			if (this->head_planet != nullptr) {
				this->head_planet->reflow(width, height);
			}
		} else {
			this->pending_resize = true;
		}
	}
}

void UniverseDisplay::do_start(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	if (this->head_planet != nullptr) {
		this->head_planet->start();
	}
}

void UniverseDisplay::do_load(CanvasAnimatedControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	Size region = this->display->Size;

	syslog(Log::Debug, "loading");

	if (this->head_planet != nullptr) {
		this->head_planet->load(args, region.Width, region.Height);
	}

	this->loaded = true;

	if (this->pending_resize) {
		this->pending_resize = false;
		if (this->head_planet != nullptr) {
			this->head_planet->reflow(region.Width, region.Height);
		}
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
			syslog(Log::Notice, L"cannot update the universe within %fms.", float(elapsed) / 10000.0F);
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
			syslog(Log::Warning, L"rendering: %s", wte->Message->Data());
		}
	}
}

void UniverseDisplay::do_stop(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	if (this->head_planet != nullptr) {
		this->head_planet->stop();
	}

	this->pending_resize = false;
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
