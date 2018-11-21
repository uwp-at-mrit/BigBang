#include "graphlet/dashboard/alarmlet.hpp"

#include "colorspace.hpp"

#include "math.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasGeometry^ make_alert_lights(float cx, float cy, float start_radius, float end_radius
	, double start_degrees, double end_degrees, size_t count, float thickness, CanvasStrokeStyle^ style) {
	CanvasPathBuilder^ lights = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double step = (end_degrees - start_degrees) / double(count - 1);
	float start_x, start_y, end_x, end_y;

	for (double degrees = start_degrees; degrees <= end_degrees; degrees += step) {
		circle_point(start_radius, degrees, &start_x, &start_y);
		circle_point(end_radius, degrees, &end_x, &end_y);

		lights->BeginFigure(cx + start_x, cy + start_y);
		lights->AddLine(cx + end_x, cy + end_y);
		lights->EndFigure(CanvasFigureLoop::Open);
	}

	return geometry_stroke(CanvasGeometry::CreatePath(lights), thickness, style);
}

/*************************************************************************************************/
Alarmlet::Alarmlet(float size) : Alarmlet(AlarmStatus::None, size) {}

Alarmlet::Alarmlet(AlarmStatus dstatus, float size) : IStatuslet(dstatus), width(size), height(size) {}

void Alarmlet::construct() {
	CanvasStrokeStyle^ light_style = make_roundcap_stroke_style(true);
	double theta = 30.0;
	float cx = this->width * 0.5F;
	float base_width = this->width * 0.85F;
	float body_width = this->width * 0.618F;
	float base_height = this->height * 0.15F;
	float base_radius = (base_width - body_width) * 0.5F;
	float base_x = (this->width - base_width) * 0.5F;
	float base_y = this->height - base_height;
	float body_x = (this->width - body_width) * 0.5F;
	float light_thickness = base_radius * 0.5F;
	float body_bottom = base_y - light_thickness;
	float hat_radius = body_width * 0.5F;
	float shadow_radius = hat_radius * 0.618F;
	float light_lradius = cx / std::cos(degrees_to_radians(theta)) - light_thickness;
	float light_sradius = hat_radius + light_thickness;
	float body_y = light_lradius + light_thickness;

	CanvasGeometry^ shadow = short_arc(cx, body_y - shadow_radius,
		cx + shadow_radius, body_y, shadow_radius, shadow_radius,
		light_thickness, light_style);

	CanvasGeometry^ parts[] = {
		rounded_rectangle(base_x, base_y, base_width, base_height * 2.0F, base_radius, base_radius),
		rectangle(body_x, body_y, body_width, body_bottom - body_y),
		segment(cx, body_y, 180.0, 360.0, hat_radius),
		make_alert_lights(cx, body_y, light_lradius, light_sradius, theta - 180.0, -theta, 7, light_thickness, light_style)
	};

	this->body = geometry_freeze(geometry_intersect(geometry_union(parts), // don't mind, it's Visual Studio's fault
		geometry_subtract(rectangle(this->width, this->height), shadow)));
}

void Alarmlet::fill_extent(float x, float y, float* w, float* h) {
    SET_BOX(w, this->width);
	SET_BOX(h, this->height);
};

void Alarmlet::prepare_style(AlarmStatus status, AlarmStyle& style) {
	switch (status) {
	case AlarmStatus::Notice: CAS_SLOT(style.color, Colours::Green); break;
	case AlarmStatus::Warning: CAS_SLOT(style.color, Colours::Yellow); break;
	case AlarmStatus::Alert: CAS_SLOT(style.color, Colours::Red); break;
	}

	CAS_SLOT(style.color, Colours::GhostWhite);
}

void Alarmlet::apply_style(AlarmStyle& style) {
	this->color = make_linear_gradient_brush(0.0F, this->height, style.color->Color);
}

void Alarmlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	brush_translate(this->color, x, y);

	ds->DrawCachedGeometry(this->body, x, y, this->color);
}
