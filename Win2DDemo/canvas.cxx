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
    auto do_display = ref new CanvasDrawHandler(this, &Win2DCanvas::do_display);
    
    this->control = gpu_canvas(parent, id, do_load, do_display);
    this->edit_sequence = 0;
    this->is_refresh_pending = false;

    this->control->PointerReleased += ref new PointerEventHandler(this, &Win2DCanvas::do_input);
    this->control->PointerPressed += ref new PointerEventHandler(this, &Win2DCanvas::do_input);
}

void Win2DCanvas::do_load(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->load(e);
}

void Win2DCanvas::do_display(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->draw(e->DrawingSession);
}

void Win2DCanvas::do_input(Object^ sender, PointerRoutedEventArgs^ e) {
    auto target = dynamic_cast<::UIElement^>(e->OriginalSource);

    if (target != nullptr) {
        auto kms = e->KeyModifiers;
        auto ppt = e->GetCurrentPoint(target);
        auto pt = ppt->Position;
        auto ppps = ppt->Properties;

        switch (e->Pointer->PointerDeviceType) {
        case PointerDeviceType::Mouse: e->Handled = this->point(pt.X, pt.Y, ppps, kms); break;
        case PointerDeviceType::Touch: e->Handled = this->touch(pt.X, pt.Y, ppps, kms); break;
        case PointerDeviceType::Pen:   e->Handled = this->paint(pt.X, pt.Y, ppps, kms); break;
        }
    }
}

bool Win2DCanvas::point(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms) {
    return false;
}

bool Win2DCanvas::touch(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms) {
    return this->point(x, y, ppps, vkms);
}

bool Win2DCanvas::paint(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms) {
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
