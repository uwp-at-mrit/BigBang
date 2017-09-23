#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "snip/storagelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

CanvasGradientStop topface_stops[3];
CanvasGradientStop body_stops[3];
CanvasGradientStop used_stops[3];

StorageTanklet::StorageTanklet(float width, float height) {
    this->width = width;
    this->height = height;

    this->label_font = make_text_format(12.0F);
}

void StorageTanklet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void StorageTanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto brush = ref new CanvasSolidColorBrush(this->info, Colors::Snow);
    ds->FillRectangle(x, y, this->width, this->height, brush);
}
