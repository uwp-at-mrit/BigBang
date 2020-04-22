#include "graphlet/aislet.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
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

#define DCAS(lv, rv) if (!flisnan(rv)) lv = (rv);

/*************************************************************************************************/
AISPositionReport::AISPositionReport(double lat, double lon) {
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
	this->length = 0.0F;
	this->width = 0.0F;
	this->gps_fl = 0.0F;
	this->gps_fw = 0.0F;
}

AISVoyageReport& AISVoyageReport::operator=(const AISVoyageReport& vr) {
	this->shipname = vr.shipname;
	this->callsign = vr.callsign;
	this->mothership_mmsi = vr.mothership_mmsi;

	DCAS(this->length, vr.length);
	DCAS(this->width, vr.width);
	DCAS(this->gps_fl, vr.gps_fl);
	DCAS(this->gps_fw, vr.gps_fw);

	return (*this);
}

/*************************************************************************************************/
AISlet::AISlet(float width, float height) : width(width), height(height) {
	this->enable_resizing(false);
	this->enable_events(true, false);
}

void AISlet::construct() {
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
		for (auto it = this->positions.begin(); it != this->positions.end(); it++) {
			AISPositionReport* pr = &(it->second);
			float2 pos = this->master_map->position_to_local(pr->geo.x, pr->geo.y, x, y);

			ds->FillEllipse(pos, 5.0F, 8.0F, Colors::RoyalBlue);
		}
	}
}

void AISlet::update_self_position(AISPositionReport* pos) {
	this->self_position = (*pos);
}

void AISlet::update_position(uint16 mmsi, AISPositionReport* pos) {
	this->positions[mmsi] = (*pos);
	this->notify_updated();
}

void AISlet::update_voyage(uint16 mmsi, AISVoyageReport* voyage, bool force_update) {
	this->voyages[mmsi] = (*voyage);

	if (force_update) {
		this->notify_updated();
	}
}
