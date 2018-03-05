#include "virtualization/keyboard.hpp"
#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::System;
using namespace Microsoft::Graphics::Canvas;

private enum KeyboardCellBox { X, Y, Width, Height };

static const size_t box_size = sizeof(float) * 4;
static const long long numpad_tap_duration = 3000000LL;

static void fill_cellbox(float* box, const KeyboardCell cell, float cellsize, float gapsize) {
	float flcol = float(cell.col);
	float flrow = float(cell.row);
	float flncol = float(cell.ncol);
	float flnrow = float(cell.nrow);
	
	box[KeyboardCellBox::X] = gapsize * (flcol + 1.0F) + cellsize * flcol;
	box[KeyboardCellBox::Y] = gapsize * (flrow + 1.0F) + cellsize * flrow;
	box[KeyboardCellBox::Width] = cellsize * flncol + gapsize * (flncol - 1.0F);
	box[KeyboardCellBox::Height] = cellsize * flnrow + gapsize * (flnrow - 1.0F);
}

/*************************************************************************************************/
Keyboard::Keyboard(IPlanet* master, const KeyboardCell* cells, unsigned int keynum)
	: IKeyboard(master), cells(cells), keynum(keynum) {
	this->enable_events(true);
}

Keyboard::~Keyboard() {
	if (this->cell_boxes != nullptr) {
		free(this->cell_boxes);
	}
}

void Keyboard::construct() {
	this->create();

	if (this->cell_boxes != nullptr) {
		free(this->cell_boxes);
	}

	this->cell_boxes = (float *)calloc(this->keynum, box_size);
	for (size_t i = 0; i < keynum; i++) {
		fill_cellbox(this->cell_boxes + (i * box_size), this->cells[i], cellsize, gapsize);
	}
}

void Keyboard::update(long long count, long long interval, long long uptime, bool is_slow) {
	if (interval > numpad_tap_duration) {
		this->uptime = uptime + interval;
	} else {
		this->uptime = uptime;
	}

	if (this->tapped) {
		if (uptime - this->taptime > numpad_tap_duration) {
			this->tapped = false;
		}
	}
}

void Keyboard::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	this->draw_before(ds, x, y, Width, Height);

	for (unsigned char i = 0; i < this->keynum; i++) {
		float* box = this->cell_boxes + (box_size * i);

		VirtualKey key = this->cells[i].key;
		float cx = x + box[KeyboardCellBox::X];
		float cy = y + box[KeyboardCellBox::Y];
		float cwidth = box[KeyboardCellBox::Width];
		float cheight = box[KeyboardCellBox::Height];

		this->draw_cell(ds, key, this->current_key == key, this->tapped, cx, cy, cwidth, cheight);
	}

	this->draw_after(ds, x, y, Width, Height);
}

void Keyboard::on_hover(float local_x, float local_y, bool shifted, bool controled) {
	if (!this->tapped) {
		this->current_key = this->find_tapped_key(local_x, local_y);
	}
}

void Keyboard::on_tap(float local_x, float local_y, bool shifted, bool controled) {
	this->current_key = this->find_tapped_key(local_x, local_y);
	this->taptime = this->uptime;
	this->tapped = true;
}

void Keyboard::on_goodbye(float local_x, float local_y, bool shifted, bool controled) {
	this->current_key = VirtualKey::None;
}

VirtualKey Keyboard::find_tapped_key(float mouse_x, float mouse_y) {
	VirtualKey found = VirtualKey::None;

	for (unsigned char i = 0; i < this->keynum; i++) {
		float* box = this->cell_boxes + (i * box_size);
		float cx = box[KeyboardCellBox::X];
		float cy = box[KeyboardCellBox::Y];
		float cwidth = box[KeyboardCellBox::Width];
		float cheight = box[KeyboardCellBox::Height];

		if ((cx < mouse_x) && (mouse_x < cx + cwidth) && (cy < mouse_y) && (mouse_y < cy + cheight)) {
			found = this->cells[i].key;
		}
	}

	return found;
}

bool Keyboard::is_colliding_with_mouse(float mouse_x, float mouse_y, float keyboard_x, float keyboard_y) {
	bool yes = false;

	if ((this->shown() && (keyboard_x < mouse_x) && (keyboard_y < mouse_y))) {
		float keyboard_width, keyboard_height;

		this->fill_extent(keyboard_x, keyboard_y, &keyboard_width, &keyboard_height);
		if ((mouse_x < keyboard_x + keyboard_width) && (mouse_y < keyboard_y + keyboard_height)) {
			yes = true;
		}
	}

	return yes;
}

/*************************************************************************************************/
IKeyboard::IKeyboard(IPlanet* master) : master(master) {
	this->show(false);
}

Syslog* IKeyboard::get_logger() {
	return this->master->get_logger();
}

void IKeyboard::show(bool shown) {
	this->_shown = shown;
}

bool IKeyboard::shown() {
	return this->_shown;
}
