#include "graphlet/aislet.hpp"

#include "datum/flonum.hpp"

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

void AISlet::update_position(uint16 mmsi, AISPositionReport* pos) {
	this->positions[mmsi] = (*pos);
	this->notify_updated();
}
