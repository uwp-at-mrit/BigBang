#include "graphlet/primitive.hpp"

#include "planet.hpp"
#include "shape.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
Windows::Foundation::Rect WarGrey::SCADA::symbol_enclosing_box(float radiusX, float radiusY, double degrees) {
	return geometry_rotate(rectangle(radiusX * 2.0F, radiusY * 2.0F), degrees)->ComputeBounds();
}

Rect WarGrey::SCADA::graphlet_enclosing_box(IGraphlet* g, float x, float y, float3x2 transform) {
	float width, height;

	g->fill_extent(x, y, &width, &height);

	return rectangle(x, y, width, height)->ComputeBounds(transform);
}

/*************************************************************************************************/
IGraphlet::~IGraphlet() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
}

IPlanet* IGraphlet::master() {
	IPlanet* planet = nullptr;

	if (this->info != nullptr) {
		planet = this->info->master;
	}

	return planet;
}

Syslog* IGraphlet::get_logger() {
	Syslog* logger = default_logger();

	if (this->info != nullptr) {
		logger = this->info->master->get_logger();
	}

	return logger;
}

void IGraphlet::notify_ready() {
	if (this->info != nullptr) {
		this->info->master->notify_graphlet_ready(this);
	}
}

void IGraphlet::notify_updated() {
	if (this->info != nullptr) {
		if (this->anchor != GraphletAnchor::LT) {
			this->info->master->move_to(this, this->anchor_x, this->anchor_y, this->anchor);
			this->clear_moor();
		}

		this->info->master->notify_graphlet_updated(this);
	}
}

void IGraphlet::moor(GraphletAnchor anchor) {
	if (anchor != GraphletAnchor::LT) {
		if (this->info != nullptr) {
			this->anchor = anchor;
			this->info->master->fill_graphlet_location(this, &this->anchor_x, &this->anchor_y, anchor);
		}
	}
}

void IGraphlet::clear_moor() {
	this->anchor = GraphletAnchor::LT;
}

bool IGraphlet::has_caret() {
	bool careted = false;

	if (this->info != nullptr) {
		careted = (this->info->master->get_focus_graphlet() == this);
	}

	return careted;
}

float IGraphlet::available_visible_width(float here_x) {
	float width = here_x;

	if (this->info != nullptr) {
		width = this->info->master->actual_width();
	}

	return width - here_x;
}

float IGraphlet::available_visible_height(float here_y) {
	float height = here_y;

	if (this->info != nullptr) {
		height = this->info->master->actual_height();
	}

	return height - here_y;
}

float IGraphlet::sketch_to_application_width(float sketch_width) {
	float width = sketch_width;

	if (this->info != nullptr) {
		width = this->info->master->sketch_to_application_width(sketch_width);
	}

	return width;
}

float IGraphlet::sketch_to_application_height(float sketch_height) {
	float height = sketch_height;

	if (this->info != nullptr) {
		height = this->info->master->sketch_to_application_height(sketch_height);
	}

	return height;
}

void IGraphlet::fill_my_location(float* x, float* y, WarGrey::SCADA::GraphletAnchor a) {
	if (this->info != nullptr) {
		this->info->master->fill_graphlet_location(this, x, y, a);
	}
}
