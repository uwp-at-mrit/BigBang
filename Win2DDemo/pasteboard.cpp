#include "ui.h"
#include "pasteboard.h"
#include "snip.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

Win2DPanel::Win2DPanel(Panel^ parent, String^ id) {
    auto onLoad = ref new CanvasLoadHandler(this, &Win2DPanel::OnLoad);
    auto onPaint = ref new CanvasDrawHandler(this, &Win2DPanel::OnPaint);
    entity = gpu_canvas(parent, id, onLoad, onPaint);
}

void Win2DPanel::OnLoad(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->LoadResources(e);
}

void Win2DPanel::OnPaint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->Draw(e->DrawingSession);
}

void Win2DPanel::ChangeSize(double width, double height) {
    entity->Width = width;
    entity->Height = height;
    this->OnDisplaySize(width, height);
}

void Win2DPanel::SmartRedraw() {
    entity->Invalidate();
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : Win2DPanel(parent, id) {    
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
