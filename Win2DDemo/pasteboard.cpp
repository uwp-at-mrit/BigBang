#include "ui.h"
#include "pasteboard.h"
#include "snip.h"

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
TextExtent Win2DCanvas::GetTextExtent(String^ message, CanvasTextFormat^ fontInfo) {
    if (sharedDrawingSession == nullptr) {
        auto sharedDevice = CanvasDevice::GetSharedDevice();
        auto target = ref new CanvasRenderTarget(sharedDevice, 1.0f, 1.0f, 96);
        sharedDrawingSession = target->CreateDrawingSession();
    }

    auto textLayout = ref new CanvasTextLayout(sharedDrawingSession, message, fontInfo, 0.0f, 0.0f);
    Rect logical = textLayout->LayoutBounds;
    Rect ink = textLayout->DrawBounds;
    float space = ink.X - logical.X;
    float distance = logical.Height - ink.Height - space;
    return TextExtent({logical.Width, logical.Height, distance, space});
}

Win2DCanvas::Win2DCanvas(Panel^ parent, String^ id) {
    auto onLoad = ref new CanvasLoadHandler(this, &Win2DCanvas::OnLoad);
    auto onPaint = ref new CanvasDrawHandler(this, &Win2DCanvas::OnPaint);
    entity = gpu_canvas(parent, id, onLoad, onPaint);
}

void Win2DCanvas::OnLoad(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->LoadResources(e);
}

void Win2DCanvas::OnPaint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->Draw(e->DrawingSession);
}

void Win2DCanvas::ChangeSize(double width, double height) {
    entity->Width = width;
    entity->Height = height;
    this->OnDisplaySize(width, height);
}

void Win2DCanvas::SmartRedraw() {
    entity->Invalidate();
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : Win2DCanvas(parent, id) {    
}

Pasteboard::~Pasteboard() {
    delete headSnip;
}

void Pasteboard::Insert(Snip* snip) {
    if (headSnip == nullptr) {
        headSnip = snip;
    }

    this->SmartRedraw();
}

void Pasteboard::LoadResources(CanvasCreateResourcesEventArgs^ e) {
}

void Pasteboard::Draw(CanvasDrawingSession^ ds) {
    if (headSnip != nullptr) {
        headSnip->Draw(ds);
    }
}
