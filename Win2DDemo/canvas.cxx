#include "ui.hxx"
#include "pasteboard.hxx"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Collections;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::Devices::Input;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

static CanvasDrawingSession^ shared_ds;

CanvasDrawingSession^ Win2D::UIElement::make_shared_drawing_session() {
    if (shared_ds == nullptr) {
        auto sharedDevice = CanvasDevice::GetSharedDevice();
        auto target = ref new CanvasRenderTarget(sharedDevice, 1.0f, 1.0f, 96);
        shared_ds = target->CreateDrawingSession();
    }

    return shared_ds;
}

TextExtent Win2D::UIElement::get_text_extent(String^ message, CanvasTextFormat^ layout_config) {
    return get_text_extent(make_shared_drawing_session(), message, layout_config);
}

TextExtent Win2D::UIElement::get_text_extent(CanvasDrawingSession^ ds, String^ message, CanvasTextFormat^ layout_config) {
    auto layout = ref new CanvasTextLayout(ds, message, layout_config, 0.0f, 0.0f);
    Rect logical = layout->LayoutBounds;
    Rect ink = layout->DrawBounds;
    float space = ink.Y - logical.Y;
    float descent = logical.Height - ink.Height - space;
    float left = ink.X - logical.X;
    float right = logical.Width - ink.Width - left;

    return { logical.Width, logical.Height, descent, space, left, right };
}

/*************************************************************************************************/
Win2DCanvas::Win2DCanvas(Panel^ parent, String^ id) {
    auto do_load = ref new CanvasLoadHandler(this, &Win2DCanvas::do_load);
    auto do_paint = ref new CanvasDrawHandler(this, &Win2DCanvas::do_paint);
    
    this->control = gpu_canvas(parent, id, do_load, do_paint);
    this->edit_sequence = 0;
    this->is_refresh_pending = false;

    this->control->PointerReleased += ref new PointerEventHandler(this, &Win2DCanvas::do_click);
    this->control->PointerMoved += ref new PointerEventHandler(this, &Win2DCanvas::do_notice);
    this->control->PointerPressed += ref new PointerEventHandler(this, &Win2DCanvas::delay_pressed);
}

void Win2DCanvas::do_load(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->load(e);
}

void Win2DCanvas::do_paint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->draw(e->DrawingSession);
}

void Win2DCanvas::do_notice(Object^ sender, PointerRoutedEventArgs^ e) {
    auto ppt = e->GetCurrentPoint(this->canvas);
    e->Handled = this->dispatch_event(ppt->Position, ppt->Properties, e->KeyModifiers, e->Pointer->PointerDeviceType);
}

void Win2DCanvas::do_click(Object^ sender, PointerRoutedEventArgs^ e) {
    auto ppt = e->GetCurrentPoint(this->canvas);
    e->Handled = this->dispatch_event(ppt->Position, this->ppps, e->KeyModifiers, e->Pointer->PointerDeviceType);
}

void Win2DCanvas::delay_pressed(Object^ sender, PointerRoutedEventArgs^ e) {
    this->ppps = e->GetCurrentPoint(this->canvas)->Properties;
}

bool Win2DCanvas::dispatch_event(Point pt, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return this->action(pt.X, pt.Y, ppps, vkms, type);
}

bool Win2DCanvas::action(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}

bool Win2DCanvas::notice(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}

/*************************************************************************************************/
void Win2DCanvas::begin_edit_sequence() {
    this->edit_sequence += 1;
}

void Win2DCanvas::end_edit_sequence() {
    this->edit_sequence -= 1;

    if (this->edit_sequence <= 0) {
        if (this->is_refresh_pending) this->refresh();
        if (this->edit_sequence < 0) this->edit_sequence = 0;
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
