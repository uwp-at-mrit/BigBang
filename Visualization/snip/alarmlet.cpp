#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>
#include <cmath>

#include "alarmlet.hpp"
#include "canvas.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;
using namespace Windows::Foundation::Numerics;

Alarmlet::Alarmlet(float size) : size(size) {}

Alarmlet::~Alarmlet() {}

void Alarmlet::fill_extent(float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    if (width != nullptr) (*width) = this->size;
    if (height != nullptr) (*height) = this->size;
    if (descent != nullptr) (*descent) = 0.0F;
    if (space != nullptr) (*space) = 0.0F;
    if (lspace != nullptr) (*lspace) = 0.0F;
    if (rspace != nullptr) (*rspace) = 0.0F;
};

void Alarmlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto alarm = ref new CanvasPathBuilder(ds);
    float base_width = this->size;
    float base_height = this->size / 8.0F;
    float base_y = this->size - base_height;
    float radius = this->size * 0.75F * 0.5F;

    alarm->BeginFigure(x + base_height, y + base_y);
    alarm->AddLine(x + base_height, y + radius);
    alarm->AddArc(float2(x + this->size / 2.0F, y + radius), radius, radius, float(M_PI), float(M_PI));
    alarm->AddLine(x + this->size - base_height, y + base_y);
    alarm->EndFigure(CanvasFigureLoop::Closed);

    ds->FillGeometry(CanvasGeometry::CreatePath(alarm), Colors::Purple);
    ds->FillRectangle(x, y + base_y, size, base_height, Colors::Black);
}
