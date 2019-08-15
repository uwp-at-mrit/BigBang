#include "virtualization/screen/pasteboard.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;

/*************************************************************************************************/
Pasteboard::Pasteboard(IDisplay^ display, DisplayFit mode, float target_width, float target_height, float source_width, float source_height)
	: IScreen(mode, target_width, target_height, source_width, source_height), _display(display) {}

Syslog* Pasteboard::get_logger() {
	return this->_display->get_logger();
}

IDisplay^ Pasteboard::display() {
	return this->_display;
}

float Pasteboard::actual_width(IPlanet* p) {
	return this->_display->planet_actual_width(p);
}

float Pasteboard::actual_height(IPlanet* p) {
	return this->_display->planet_actual_height(p);
}

void Pasteboard::min_resize(float width, float height) {
	if (width > 0.0F) {
		this->_display->min_width = width;
	}

	if (height > 0.0F) {
		this->_display->min_height = height;
	}
}

float Pasteboard::min_width() {
	return this->_display->min_width;
}

float Pasteboard::min_height() {
	return this->_display->min_height;
}

void Pasteboard::view_resize(float width, float height) {
	this->min_resize(width, height);	
}

float Pasteboard::view_width() {
	return this->_display->actual_width;
}

float Pasteboard::view_height() {
	return this->_display->actual_height;
}

bool Pasteboard::surface_ready() {
	return this->_display->surface_ready();
}

bool Pasteboard::ui_thread_ready() {
	return this->_display->ui_thread_ready();
}

bool Pasteboard::shown() {
	return this->_display->shown();
}

void Pasteboard::refresh(IPlanet* planet) {
	this->_display->refresh(planet);
}

Point Pasteboard::global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff, float yoff) {
	return this->_display->global_to_local_point(p, global_x, global_y, xoff, yoff);
}

Point Pasteboard::local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff, float yoff) {
	return this->_display->local_to_global_point(p, local_x, local_y, xoff, yoff);
}


void Pasteboard::begin_update_sequence() {
	this->update_sequence_depth += 1;
}

bool Pasteboard::in_update_sequence() {
	return (this->update_sequence_depth > 0);
}

void Pasteboard::end_update_sequence() {
	this->update_sequence_depth -= 1;

	if (this->update_sequence_depth < 1) {
		this->update_sequence_depth = 0;

		if (this->update_is_needed) {
			this->refresh(nullptr);
			this->update_is_needed = false;
		}
	}
}

bool Pasteboard::needs_update() {
	return this->update_is_needed;
}

void Pasteboard::notify_graphlet_updated(ISprite* g) { // NOTE: `g` may be `nullptr`
	if (this->in_update_sequence()) {
		this->update_is_needed = true;
	} else {
		this->refresh(nullptr);
		this->update_is_needed = false;
	}
}

void Pasteboard::enter_critical_section() {
	this->section.lock();
}

void Pasteboard::enter_shared_section() {
	this->section.lock_shared();
}

void Pasteboard::leave_critical_section() {
	this->section.unlock();
}

void Pasteboard::leave_shared_section() {
	this->section.unlock_shared();
}
