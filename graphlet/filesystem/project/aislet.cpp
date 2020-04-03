#include "graphlet/filesystem/project/aislet.hpp"

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
AISlet::AISlet() : master(nullptr) {
	this->enable_resizing(false);
	this->enable_events(true, false);
}

void AISlet::construct() {
}

void AISlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(y));
}

bool AISlet::is_colliding_with_mouse(float local_x, float local_y) {
	return false;
}

void AISlet::on_tap(float local_x, float local_y) {
}

void AISlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	
}

void AISlet::attach_to_map(DigMaplet* master, bool force) {
	if (master != nullptr) {
		if (force || (this->master != master)) {	
			this->notify_updated();
		}
	}

	this->master = master;
}

void AISlet::on_vessel_move(double vessel_x, double vessel_y) {
	
}
