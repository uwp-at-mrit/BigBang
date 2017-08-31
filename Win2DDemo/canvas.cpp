#include "ui.hpp"
#include "pasteboard.hpp"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::Foundation;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

static CanvasDrawingSession^ sharedDrawingSession;

CanvasDrawingSession^ Win2D::UIElement::make_shared_drawing_session() {
    if (sharedDrawingSession == nullptr) {
        auto sharedDevice = CanvasDevice::GetSharedDevice();
        auto target = ref new CanvasRenderTarget(sharedDevice, 1.0f, 1.0f, 96);
        sharedDrawingSession = target->CreateDrawingSession();
    }

    return sharedDrawingSession;
}

TextExtent Win2D::UIElement::get_text_extent(String^ message, CanvasTextFormat^ fontInfo) {
    return get_text_extent(make_shared_drawing_session(), message, fontInfo);
}

TextExtent Win2D::UIElement::get_text_extent(CanvasDrawingSession^ ds, String^ message, CanvasTextFormat^ fontInfo) {
    auto textLayout = ref new CanvasTextLayout(ds, message, fontInfo, 0.0f, 0.0f);
    Rect logical = textLayout->LayoutBounds;
    Rect ink = textLayout->DrawBounds;
    float space = ink.Y - logical.Y;
    float descent = logical.Height - ink.Height - space;
    float left = ink.X - logical.X;
    float right = logical.Width - ink.Width - left;

    return { logical.Width, logical.Height, descent, space, left, right };
}

/*************************************************************************************************/
Win2DCanvas::Win2DCanvas(Panel^ parent, String^ id) {
    auto onLoad = ref new CanvasLoadHandler(this, &Win2DCanvas::OnLoad);
    auto onPaint = ref new CanvasDrawHandler(this, &Win2DCanvas::OnPaint);
    
    this->control = gpu_canvas(parent, id, onLoad, onPaint);
    this->editSequence = 0;
    this->isRefreshPending = false;
}

void Win2DCanvas::OnLoad(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->LoadResources(e);
}

void Win2DCanvas::OnPaint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->Draw(e->DrawingSession);
}

void Win2DCanvas::BeginEditSequence() {
    this->editSequence += 1;
}

void Win2DCanvas::EndEditSequence() {
    this->editSequence -= 1;

    if (this->editSequence <= 0) {
        if (this->isRefreshPending) this->Refresh();
        if (this->editSequence < 0) this->editSequence = 0;
    }
}

void Win2DCanvas::Refresh() {
    if (this->editSequence > 0) {
        this->isRefreshPending = true;
    } else {
        this->control->Invalidate();
        this->isRefreshPending = false;
    }
}
