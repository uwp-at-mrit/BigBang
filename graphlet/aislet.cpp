#include "graphlet/aislet.hpp"

#include "datum/string.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
#include "polar.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::UI;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
#define DCAS(lv, rv) if (!flisnan(rv)) lv = (rv);

static CanvasSolidColorBrush^ default_vessel_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ vessel_class_a_color = Colours::Green;
static CanvasSolidColorBrush^ vessel_class_b_color = Colours::Yellow;

/*************************************************************************************************/
AISPositionReport::AISPositionReport(AISType type, double lat, double lon) {
	this->type = type;

	this->latitude = lat;
	this->longitude = lon;
	this->turn = flnan;
	this->speed = flnan;
	this->course = flnan;
	this->heading = flnan;

	this->geo.x = flnan;
	this->geo.y = flnan;
}

AISPositionReport& AISPositionReport::operator=(const AISPositionReport& pr) {
	this->type = pr.type;

	DCAS(this->latitude, pr.latitude);
	DCAS(this->longitude, pr.longitude);
	DCAS(this->turn, pr.turn);
	DCAS(this->speed, pr.speed);
	DCAS(this->course, pr.course);
	DCAS(this->heading, pr.heading);

	DCAS(this->geo.x, pr.geo.x);
	DCAS(this->geo.y, pr.geo.y);

	return (*this);
}

AISVoyageReport::AISVoyageReport(std::string shipname, std::string callsign) {
	this->shipname = shipname;
	this->callsign = callsign;

	this->mothership_mmsi = -1;
	this->to_bow = 0.0F;
	this->to_stern = 0.0F;
	this->to_port = 0.0F;
	this->to_starboard = 0.0F;
}

AISVoyageReport& AISVoyageReport::operator=(const AISVoyageReport& vr) {
	this->shipname = vr.shipname;
	this->callsign = vr.callsign;
	this->mothership_mmsi = vr.mothership_mmsi;

	DCAS(this->to_bow, vr.to_bow);
	DCAS(this->to_stern, vr.to_stern);
	DCAS(this->to_port, vr.to_port);
	DCAS(this->to_starboard, vr.to_starboard);

	return (*this);
}

/*************************************************************************************************/
AISlet::AISlet(float width, float height) : width(width), height(height) {
	this->enable_resizing(false);
	this->enable_events(true, false);

	this->geo_x = flnan;
	this->geo_y = flnan;
}

void AISlet::construct() {
	this->caption_font = make_bold_text_format(10.0F);
	this->distance_font = make_bold_text_format(10.0F);

	this->default_vessel = polar_triangle(10.0F, 50.0F, -90.0);
}

void AISlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

bool AISlet::is_colliding_with_mouse(float local_x, float local_y) {
	return false;
}

void AISlet::on_tap(float local_x, float local_y) {
}

void AISlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->master_map != nullptr) {
		float ds_x = x;
		float ds_y = y;
		float ds_rx = x + Width;
		float ds_by = y + Height;

		for (auto it = this->positions.begin(); it != this->positions.end(); it++) {
			AISPositionReport* pr = &(it->second);
			float2 pos = this->master_map->position_to_local(pr->geo.x, pr->geo.y, x, y);
			
			if (flin(ds_x, pos.x, ds_rx) && flin(ds_y, pos.y, ds_by)) {
				auto maybe_vr = this->voyages.find(it->first);
				auto shape = this->vessels.find(it->first);
				auto caption = this->captions.find(it->first);
				double distance = points_distance(pr->geo.x, pr->geo.y, this->geo_x, this->geo_y);
				CanvasGeometry^ vessel = ((shape == this->vessels.end()) ? this->default_vessel : shape->second);
				CanvasSolidColorBrush^ vessel_color = default_vessel_color;

				if (maybe_vr != this->voyages.end()) {
					switch (pr->type) {
					case AISType::A: vessel_color = vessel_class_a_color; break;
					case AISType::B: vessel_color = vessel_class_b_color; break;
					}
				}

				{ // display vessel
					float s = float(this->master_map->actual_scale());
					CanvasGeometry^ g = geometry_scale(geometry_rotate(vessel, pr->heading, 0.0F, 0.0F), s, s);

					ds->DrawGeometry(g, pos, vessel_color);
				}

				if (caption != this->captions.end()) {
					ds->DrawCachedGeometry(caption->second, pos, vessel_color);
				}

				if (!flisnan(distance)) {
					CanvasTextLayout^ d = make_text_layout(flstring(distance, 1), this->distance_font);

					ds->DrawTextLayout(d,
						pos.x - d->LayoutBounds.Width * 0.5F,
						pos.y + d->LayoutBounds.Height * 1.5F,
						vessel_color);
				}
			}
		}
	}
}

void AISlet::update_self_position(double x, double y) {
	DCAS(this->geo_x, x);
	DCAS(this->geo_y, y);
}

void AISlet::update_position(uint16 mmsi, AISPositionReport* pos) {
	this->positions[mmsi] = (*pos);
	this->notify_updated();
}

void AISlet::update_voyage(uint16 mmsi, AISVoyageReport* voyage, bool force_update) {
	this->voyages[mmsi] = (*voyage);

	if (this->vessels.find(mmsi) == this->vessels.end()) { // make shape
		if (voyage->mothership_mmsi < 0) {
			this->vessels[mmsi] = this->make_ship_shape(voyage);
			force_update = true;
		} else if (this->vessels.find(voyage->mothership_mmsi) != this->vessels.end()) {
			this->vessels[mmsi] = this->vessels[voyage->mothership_mmsi];
			force_update = true;
		}
	}

	if (this->captions.find(mmsi) == this->captions.end()) { // make caption
		Platform::String^ name = nullptr;

		if (voyage->shipname.size() > 0) {
			name = make_wstring(voyage->shipname);
		} else if (voyage->callsign.size() > 0) {
			name = make_wstring(voyage->callsign);
		} else {
			name = "_";
		}

		if (name != nullptr) {
			this->captions[mmsi] = this->make_ship_info(name, this->caption_font);
			force_update = true;
		}
	}

	if (force_update) {
		this->notify_updated();
	}
}

CanvasCachedGeometry^ AISlet::make_ship_info(Platform::String^ text, CanvasTextFormat^ font) {
	TextExtent te;
	CanvasGeometry^ caption = paragraph(text, font, &te);

	return geometry_freeze(geometry_translate(caption, -te.width * 0.5F, te.height * 0.5F));
}

CanvasGeometry^ AISlet::make_ship_shape(AISVoyageReport* voyage) {
	CanvasPathBuilder^ vessel = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float sb = float(voyage->to_starboard);
	float bw = float(voyage->to_bow);
	float ps = float(voyage->to_port);
	float sn = float(voyage->to_stern);
	float tx = (sb - ps) * 0.5F;
	float ty = bw - (bw - sn) * 0.382F;

	// NOTE: the y-axis of screen is downward

	vessel->BeginFigure(sb, -ty);
	vessel->AddLine(tx, -bw);
	vessel->AddLine(-ps, -ty);
	vessel->AddLine(-ps, sn);
	vessel->AddLine(sb, sn);
	vessel->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(vessel);
}
