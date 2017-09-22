#include <cmath>

#include "text.hpp"
#include "time.hpp"
#include "tongue.hpp"
#include "snip/gaugelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::ApplicationModel::Resources;

using namespace Windows::Devices::Power;
using namespace Windows::Devices::WiFi;

using namespace Windows::Networking;
using namespace Windows::Networking::Connectivity;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Gaugelet::Gaugelet(Platform::String^ caption, float A, int n) : ampere(A), rpm(n) {
    this->caption = caption;
    this->label_font = make_text_format(12.0F);
    this->scale_font = make_text_format(9.0F);
}

void Gaugelet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    TextExtent ts = get_text_extent(this->caption, this->label_font);

    SET_BOX(w, ts.width);
    SET_BOX(h, ts.height + 200);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Gaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    TextExtent ts = get_text_extent(this->caption, this->label_font);

    ds->DrawText(this->caption, x, y, Colors::White, this->label_font);
    ds->FillRectangle(x, y + ts.height, 140, 200, Colors::DarkGray);
}
