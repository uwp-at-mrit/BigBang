#include "ui.hpp"
#include "pasteboard.hxx"

using namespace WarGrey::SCADA;
using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;

using namespace Windows::System;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

Win2DCanvas::Win2DCanvas(Panel^ parent, Platform::String^ id) {
    auto do_load = ref new CanvasLoadHandler(this, &Win2DCanvas::do_load);
    auto do_paint = ref new CanvasDrawHandler(this, &Win2DCanvas::do_paint);
    
    this->control = gpu_canvas(parent, id, do_load, do_paint);
    this->edit_sequence = 0;
    this->is_refresh_pending = false;
}

void Win2DCanvas::do_load(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->load(e);
}

void Win2DCanvas::do_paint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->draw(e->DrawingSession);
}

/*************************************************************************************************/
void Win2DCanvas::resize(double width, double height) {
    this->canvas->Width = width;
    this->canvas->Height = height;
}

void Win2DCanvas::begin_edit_sequence() {
    this->edit_sequence += 1;
}

void Win2DCanvas::end_edit_sequence() {
    this->edit_sequence -= 1;

    if (this->edit_sequence <= 0) {
        if (this->is_refresh_pending) this->refresh();
        if (this->edit_sequence < 0) this->edit_sequence = 0;
        this->on_end_edit_sequence();
    }
}

void Win2DCanvas::refresh() {
    if (this->edit_sequence > 0) {
        this->is_refresh_pending = true;
    } else {
        this->control->Invalidate();
        this->is_refresh_pending = false;
    }
}

/*************************************************************************************************/
CanvasControl^ Win2DCanvas::canvas::get() { return control; }
float Win2DCanvas::actual_width::get() { return float(this->canvas->ActualWidth); };
float Win2DCanvas::actual_height::get() { return float(this->canvas->ActualHeight); };

void Win2DCanvas::max_canvas_width::set(float v) { this->canvas->MaxWidth = double(v); }
float Win2DCanvas::max_canvas_width::get() { return float(this->canvas->MaxWidth); };

void Win2DCanvas::max_canvas_height::set(float v) { this->canvas->MaxHeight = double(v); }
float Win2DCanvas::max_canvas_height::get() { return float(this->canvas->MaxHeight); };

void Win2DCanvas::min_canvas_width::set(float v) { this->canvas->MinWidth = double(v); }
float Win2DCanvas::min_canvas_width::get() { return float(this->canvas->MinWidth); };

void Win2DCanvas::min_canvas_height::set(float v) { this->canvas->MinHeight = double(v); }
float Win2DCanvas::min_canvas_height::get() { return float(this->canvas->MinHeight); };

void Win2DCanvas::canvas_width::set(float v) { this->canvas->Width = double(v); }
float Win2DCanvas::canvas_width::get() { return float(this->canvas->Width); };

void Win2DCanvas::canvas_height::set(float v) { this->canvas->Height = double(v); }
float Win2DCanvas::canvas_height::get() { return float(this->canvas->Height); };
