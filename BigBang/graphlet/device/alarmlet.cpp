#include "graphlet/device/alarmlet.hpp"

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
	float start_x, start_y, end_x, end_y;
	double step = (end_degrees - start_degrees) / double(count - 1);

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
Alarmlet::Alarmlet(float width) : Alarmlet(AlarmStatus::Normal, width) {}

Alarmlet::Alarmlet(AlarmStatus dstatus, float width) : IStatuslet(dstatus), width(width), height(width * 1.2F) {
	this->update_status();
}

void Alarmlet::construct() {
	CanvasStrokeStyle^ light_style = ref new CanvasStrokeStyle();
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
	float light_sradius = hat_radius + light_thickness * 1.5F;
	float body_y = light_lradius + light_thickness;
	
	light_style->StartCap = CanvasCapStyle::Round;
	light_style->EndCap = CanvasCapStyle::Round;

	CanvasGeometry^ shadow = short_arc(cx, body_y - shadow_radius,
		cx + shadow_radius, body_y,
		shadow_radius, shadow_radius,
		light_thickness, light_style);

	CanvasGeometry^ parts[] = {
		rounded_rectangle(base_x, base_y, base_width, base_height * 2.0F, base_radius, base_radius),
		rectangle(body_x, body_y, body_width, body_bottom - body_y),
		sector(cx, body_y, 180.0, 360.0, hat_radius),
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
	case AlarmStatus::Alert: CAS_SLOT(style.color, Colours::Firebrick); break;
	}

	CAS_SLOT(style.color, Colours::GhostWhite);
}

void Alarmlet::apply_style(AlarmStyle& style) {
	Color colors[2];

	colors[0] = scale_color(style.color->Color, 1.618);
	colors[1] = scale_color(style.color->Color, 0.618);

	this->color = make_linear_gradient_brush(0, this->height, make_gradient_stops(colors)); // Don't mind, it Visual Studio's fault
}

void Alarmlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	brush_translate(this->color, x, y);

	ds->DrawCachedGeometry(this->body, x, y, this->color);
}
