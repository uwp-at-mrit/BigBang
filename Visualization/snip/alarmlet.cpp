#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>
#include <cmath>

#include "alarmlet.hpp"
#include "canvas.hxx"

using namespace WarGrey::WinACS;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Media;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;
using namespace Windows::Foundation::Numerics;

Alarmlet::~Alarmlet() {}

Alarmlet::Alarmlet(float size) : size(size) {}

SnipTypes Alarmlet::get_type() {
    return SnipTypes::Text;
}

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

/*************************************************************************************************/
SnipIcon* WarGrey::WinACS::make_alarmlet_icon(float size, Windows::UI::Color color) {
    return new AlarmIcon(size, color);
}

AlarmIcon::AlarmIcon(float size, Windows::UI::Color color) : SnipIcon(size, color) {}

AlarmIcon::~AlarmIcon() {}

void AlarmIcon::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float inset = 8.0F;

    float base_size = SnipIcon::size - inset * 2.0F;
    float base_height = base_size / 8.0F;
    float base_x = x + inset;
    float base_y = y + inset + base_size - base_height;
    
    float radius = base_size * 0.375F;
    float alarm_x = base_x + base_height;
    float center_x = alarm_x + radius;
    float center_y = y + inset + radius;

    auto alarm = ref new CanvasPathBuilder(ds);
    alarm->BeginFigure(alarm_x, base_y);
    alarm->AddLine(alarm_x, center_y);
    alarm->AddArc(float2(center_x, center_y), radius, radius, float(M_PI), float(M_PI));
    alarm->AddLine(alarm_x + radius * 2.0F, base_y);
    alarm->EndFigure(CanvasFigureLoop::Closed);

    ds->DrawGeometry(CanvasGeometry::CreatePath(alarm), SnipIcon::color, 2.0F);
    ds->DrawRectangle(base_x, base_y, base_size, base_height, SnipIcon::color, 2.0F);
    ds->DrawRectangle(x, y, this->size, this->size, SnipIcon::color);
}

Snip* AlarmIcon::create_snip() {
    return new Alarmlet(this->size * 2.0F);
}
