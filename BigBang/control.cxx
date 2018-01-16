#include "control.hxx"
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
UserControl^ Win2DControl::canvas::get() { return this->control; };
CanvasDevice^ Win2DControl::device::get() { return this->universe->Device; };

float Win2DControl::actual_width::get() { return float(this->control->ActualWidth); };
float Win2DControl::actual_height::get() { return float(this->control->ActualHeight); };

void Win2DControl::max_width::set(float v) { this->control->MaxWidth = double(v); }
float Win2DControl::max_width::get() { return float(this->control->MaxWidth); };

void Win2DControl::max_height::set(float v) { this->control->MaxHeight = double(v); }
float Win2DControl::max_height::get() { return float(this->control->MaxHeight); };

void Win2DControl::min_width::set(float v) { this->control->MinWidth = double(v); }
float Win2DControl::min_width::get() { return float(this->control->MinWidth); };

void Win2DControl::min_height::set(float v) { this->control->MinHeight = double(v); }
float Win2DControl::min_height::get() { return float(this->control->MinHeight); };

void Win2DControl::width::set(float v) { this->control->Width = double(v); }
float Win2DControl::width::get() { return float(this->control->Width); };

void Win2DControl::height::set(float v) { this->control->Height = double(v); }
float Win2DControl::height::get() { return float(this->control->Height); };

/*************************************************************************************************/
Win2DUniverse::Win2DUniverse(SplitView^ parent, int frame_rate, Platform::String^ id) : parent(parent) {
	this->planet = ref new CanvasAnimatedControl();
	if (id != nullptr) this->planet->Name = id;

	if (frame_rate > 0) {
		this->planet->TargetElapsedTime = TimeSpan({ 10000000LL / frame_rate });
	} else if (frame_rate < 0) {
		this->planet->TargetElapsedTime = TimeSpan({ -10000000LL * frame_rate });
	}

	this->planet->UseSharedDevice = true; // this is required
	this->planet->SizeChanged += ref new SizeChangedEventHandler(this, &Win2DUniverse::do_resize);
	this->planet->CreateResources += ref new UniverseLoadHandler(this, &Win2DUniverse::do_load);
	this->planet->GameLoopStarting += ref new UniverseHandler(this, &Win2DUniverse::do_start);
	this->planet->GameLoopStopped += ref new UniverseHandler(this, &Win2DUniverse::do_stop);
	this->planet->Update += ref new UniverseUpdateHandler(this, &Win2DUniverse::do_update);
	this->planet->Draw += ref new UniverseDrawHandler(this, &Win2DUniverse::do_paint);

	this->planet->PointerMoved += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_moved);
	this->planet->PointerPressed += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_pressed);
	this->planet->PointerReleased += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_released);

	Win2DControl::universe = this->planet;
	Win2DControl::control = this->planet;

	this->parent->Content = this->planet;
}

float Win2DUniverse::actual_width::get() { return float(this->planet->Size.Width); };
float Win2DUniverse::actual_height::get() { return float(this->planet->Size.Height); };

void Win2DUniverse::do_resize(Platform::Object^ sender, SizeChangedEventArgs^ args) {
	float width = args->NewSize.Width;
	float height = args->NewSize.Height;

	if ((width > 0.0F) && (height > 0.0F)) {
		if (this->loaded) {
			//this->world->reflow(width, height);
		} else {
			this->pending_resize = true;
		}
	}
}

void Win2DUniverse::do_start(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	syslog(Log::Debug, "start");
	//this->world->start();
}

void Win2DUniverse::do_load(CanvasAnimatedControl^ sender, CanvasCreateResourcesEventArgs^ args) {
	Size region = this->planet->Size;

	//this->world->load(args, region.Width, region.Height);
	this->loaded = true;

	if (this->pending_resize) {
		this->pending_resize = false;
		//this->world->reflow(region.Width, region.Height);
	}
}

void Win2DUniverse::do_update(ICanvasAnimatedControl^ sender, CanvasAnimatedUpdateEventArgs^ args) {
	long long count = args->Timing.UpdateCount - 1;
	long long elapsed = args->Timing.ElapsedTime.Duration;
	long long uptime = args->Timing.TotalTime.Duration;
	bool is_slow = args->Timing.IsRunningSlowly;

	//this->world->update(count, elapsed, uptime, is_slow);

	if (is_slow) {
		syslog(Log::Notice, L"cannot update the universe within %fms.", float(elapsed) / 10000.0F);
	}
}

void Win2DUniverse::do_paint(ICanvasAnimatedControl^ sender, CanvasAnimatedDrawEventArgs^ args) {
	Size region = this->planet->Size;

	try {
		//this->world->enter_critical_section();
		//this->world->draw(args->DrawingSession, region.Width, region.Height);
		//this->world->leave_critical_section();
	} catch (Platform::Exception^ wte) {
		//this->world->leave_critical_section();
		syslog(Log::Warning, L"rendering: %s", wte->Message->Data());
	}
}

void Win2DUniverse::do_stop(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
	// this->world->stop();
	this->pending_resize = false;
}

void Win2DUniverse::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	// this->world->on_pointer_moved(Win2DControl::canvas, args);
}

void Win2DUniverse::on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	// this->world->on_pointer_pressed(Win2DControl::canvas, args);
}

void Win2DUniverse::on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	// this->world->on_pointer_released(Win2DControl::canvas, args);
}
