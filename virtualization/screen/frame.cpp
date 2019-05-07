#include "virtualization/screen/frame.hpp"

#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;

/*************************************************************************************************/
Frame::Frame(IGraphlet* display, DisplayFit mode, float target_width, float target_height, float source_width, float source_height)
	: IScreen(display->get_logger(), mode, target_width, target_height, source_width, source_height), _display(display) {}

IDisplay^ Frame::display() {
	return this->_display->master()->master()->display();
}

float Frame::actual_width(IPlanet* p) {
	return this->view_width();
}

float Frame::actual_height(IPlanet* p) {
	return this->view_height();
}

void Frame::min_resize(float width, float height) {
	this->_display->resize(width, height);
}

float Frame::min_width() {
	return this->min_width();
}

float Frame::min_height() {
	return this->view_height();
}

void Frame::view_resize(float width, float height) {
	this->min_resize(width, height);	
}

float Frame::view_width() {
	float width = 0.0F;

	this->_display->fill_extent(0.0F, 0.0F, &width, nullptr);

	return width;
}

float Frame::view_height() {
	float height = 0.0F;

	this->_display->fill_extent(0.0F, 0.0F, nullptr, &height);

	return height;
}

bool Frame::surface_ready() {
	return this->_display->info->master->surface_ready();
}

bool Frame::ui_thread_ready() {
	return this->_display->info->master->ui_thread_ready();
}

bool Frame::shown() {
	return this->_display->info->master->shown();
}

void Frame::refresh(IPlanet* planet) {
	this->_display->notify_updated();
}

Point Frame::global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff, float yoff) {
	IPlanet* master = this->_display->master();
	Point local_global = master->master()->global_to_local_point(p, global_x, global_y, xoff, yoff);
	float x, y;

	master->fill_graphlet_location(this->_display, &x, &y, GraphletAnchor::LT);

	return Point(local_global.Y - x, local_global.Y - y);
}

Point Frame::local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff, float yoff) {
	IPlanet* master = this->_display->master();
	Point global_local = master->master()->local_to_global_point(p, local_x, local_y, xoff, yoff);
	float x, y;

	master->fill_graphlet_location(this->_display, &x, &y, GraphletAnchor::LT);

	return Point(global_local.X + x, global_local.Y + y);
}
