#include "graphlet/filesystem/configuration/aislet.hpp"

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

static const float defaut_vessel_half_length = 50.0F;
static const float defaut_vessel_half_width = 20.0F;

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
	this->course_rad = flnan_f;
}

void AISlet::construct() {
	TextExtent metainfo_te;

	this->caption_font = make_bold_text_format(10.0F);
	this->distance_font = make_bold_text_format(10.0F);
	this->metainfo_font = make_bold_text_format(10.0F);

	this->default_vessel = polar_triangle(defaut_vessel_half_width, defaut_vessel_half_length, -90.0);
}

void AISlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void AISlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->master_map != nullptr) {
		float ds_x = x;
		float ds_y = y;
		float ds_rx = x + Width;
		float ds_by = y + Height;
		double geo_ux, geo_uy;
		float2 self_pos;

		dot_unit_vector(this->geo_x, this->geo_y, this->course_rad, &geo_ux, &geo_uy);
		self_pos = this->master_map->position_to_local(this->geo_x, this->geo_y, x, y);

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
					float scale = float(this->master_map->actual_scale());
					double heading = pr->heading;
					
					if (flisnan(heading)) {
						heading = (flisnan(pr->course) ? 0.0 : pr->course);
					}

					ds->DrawGeometry(geometry_scale(geometry_rotate(vessel, heading, 0.0F, 0.0F), scale, scale), pos, vessel_color);
				}

				if (caption != this->captions.end()) {
					ds->DrawCachedGeometry(caption->second, pos, vessel_color);
				}

				/*
				if (!flisnan(pr->course)) {
					double ux, uy, ix, iy, t1, t2;

					dot_unit_vector(pr->geo.x, pr->geo.y, pr->course, &ux, &uy);

					lines_intersection(this->geo_x, this->geo_y, geo_ux, geo_uy, pr->geo.x, pr->geo.y, ux, uy, &ix, &iy, &t1, &t2);

					if ((t1 > 0.0) && (t2 > 0.0)) {
						float2 intersection = this->master_map->position_to_local(ix, iy, x, y);

						ds->DrawLine(pos.x, pos.y, intersection.x, intersection.y, Colours::Chocolate);
						ds->DrawLine(self_pos.x, self_pos.y, intersection.x, intersection.y, Colours::Chocolate);
					}
				}
				*/
			}
		}
	}
}

bool AISlet::is_colliding_with_mouse(float local_x, float local_y) {
	bool colliding = false;

	/*
	if (this->master_map != nullptr) {
		float ds_x = 0.0;
		float ds_y = 0.0;
		float ds_rx = this->width;
		float ds_by = this->height;
		
		for (auto it = this->positions.begin(); it != this->positions.end(); it++) {
			AISPositionReport* pr = &(it->second);
			float2 pos = this->master_map->position_to_local(pr->geo.x, pr->geo.y, 0.0F, 0.0F);

			if (flin(ds_x, pos.x, ds_rx) && flin(ds_y, pos.y, ds_by)) {
				auto shape = this->vessels.find(it->first);
				
				CanvasGeometry^ vessel = ((shape == this->vessels.end()) ? this->default_vessel : shape->second);
				CanvasSolidColorBrush^ vessel_color = default_vessel_color;
				float s = float(this->master_map->actual_scale());
				CanvasGeometry^ g = geometry_scale(geometry_rotate(vessel, pr->heading, 0.0F, 0.0F), s, s);

				if (g->FillContainsPoint(pos)) {
					this->get_logger()->log_message(WarGrey::GYDM::Log::Info, L"Selected %u", it->first);
					colliding = true;
					break;
				}
			}
		}
	}
	*/

	return colliding;
}

void AISlet::on_tap(float local_x, float local_y) {

}

void AISlet::update_self_position(double x, double y) {
	if ((!flisnan(x)) && (!flisnan(y))) {
		if ((this->geo_x != x) || (this->geo_y != y)) {
			this->geo_x = x;
			this->geo_y = y;
			this->notify_updated();
		}
	}
}

void AISlet::update_self_course(double degrees) {
	if (!flisnan(degrees)) {
		float radians = degrees_to_radians(degrees);

		if (this->course_rad != radians) {
			this->course_rad = radians;
			this->notify_updated();
		}
	}
}

void AISlet::update_position(uint16 mmsi, AISPositionReport* pos) {
	this->positions[mmsi] = (*pos);
	this->notify_updated();
}

void AISlet::update_voyage(uint16 mmsi, AISVoyageReport* voyage, bool force_update) {
	this->voyages[mmsi] = (*voyage);

	if (this->vessels.find(mmsi) == this->vessels.end()) { // make shape
		if (voyage->mothership_mmsi < 0) {
			CanvasGeometry^ vessel = this->make_ship_shape(voyage);

			if (vessel != nullptr) {
				this->vessels[mmsi] = vessel;
				force_update = true;
			}
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
			name = "[no name]";
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
	CanvasPathBuilder^ vessel = nullptr;
	float ps = float(voyage->to_port);
	float sn = float(voyage->to_stern);
	float sl = float(voyage->to_bow) + sn;
	float sw = ps + float(voyage->to_starboard);

	if ((sl > 0.0) && (sw > 0.0)) {
		float tx = sw * 0.5F;
		float ty = sl * 0.618F;

		vessel = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

		// NOTE: the y-axis of win2d/direct2d is downward

		vessel->BeginFigure(0.0F, 0.0F);
		vessel->AddLine(sw, 0.0F);
		vessel->AddLine(sw, -tx);
		vessel->AddLine(tx, -sl);
		vessel->AddLine(0.0F, -tx);
		vessel->EndFigure(CanvasFigureLoop::Closed);
	}

	return (vessel == nullptr) ? this->default_vessel : geometry_translate(CanvasGeometry::CreatePath(vessel), -ps, -sn);
}
