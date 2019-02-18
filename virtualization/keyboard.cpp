#include "virtualization/keyboard.hpp"
#include "planet.hpp"

#include "paint.hpp"
#include "colorspace.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

static const long long keyboard_tap_duration = 2000000LL;

static void fill_cellbox(Rect& box, const KeyboardCell cell, float cellsize, float gapsize, float* width, float* height) {
	float flcol = float(cell.col);
	float flrow = float(cell.row);
	float flncol = float(cell.ncol);
	float flnrow = float(cell.nrow);
	
	box.X = gapsize * (flcol + 1.0F) + cellsize * flcol;
	box.Y = gapsize * (flrow + 1.0F) + cellsize * flrow;
	box.Width = cellsize * flncol + gapsize * (flncol - 1.0F);
	box.Height = cellsize * flnrow + gapsize * (flnrow - 1.0F);

	(*width) = std::fmaxf((*width), box.X + box.Width + gapsize);
	(*height) = std::fmaxf((*height), box.Y + box.Height + gapsize);
}

/*************************************************************************************************/
Keyboard::Keyboard(IPlanet* master, const KeyboardCell* cells, unsigned int keynum)
	: IKeyboard(master), cells(cells), keynum(keynum) {
	this->enable_events(true);
}

Keyboard::~Keyboard() {
	if (this->cell_boxes != nullptr) {
		delete [] this->cell_boxes;
	}
}

void Keyboard::sprite() {
	if (this->cell_boxes != nullptr) {
		delete [] this->cell_boxes;
	}

	this->cell_boxes = new Rect[this->keynum];
	for (size_t i = 0; i < this->keynum; i++) {
		fill_cellbox(this->cell_boxes[i], this->cells[i], cellsize, gapsize,
			&this->width, &this->height);
	}
}

void Keyboard::construct() {
	Color fg = Colours::Background->Color;
	Color bg = Colours::Foreground->Color;

	this->foreground = make_solid_brush(fg, 1.0);
	this->background = make_solid_brush(rgba(bg, 0.8));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));
}

void Keyboard::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Keyboard::update(long long count, long long interval, long long uptime) {
	if (interval > keyboard_tap_duration) {
		this->uptime = uptime + interval;
	} else {
		this->uptime = uptime;
	}

	if (this->tapped) {
		if (uptime - this->taptime > keyboard_tap_duration) {
			this->master->notify_graphlet_updated(this);
			this->tapped = false;
		}
	}
}

void Keyboard::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	this->draw_before(ds, x, y, Width, Height);

	if (this->cell_boxes != nullptr) {
		for (unsigned char i = 0; i < this->keynum; i++) {
			Rect box = this->cell_boxes[i];

			VirtualKey key = this->cells[i].key;
			float cx = x + box.X;
			float cy = y + box.Y;
			float cwidth = box.Width;
			float cheight = box.Height;

			this->draw_cell(ds, key, this->current_key == key, this->tapped, cx, cy, cwidth, cheight);
		}
	}

	this->draw_after(ds, x, y, Width, Height);
}

void Keyboard::draw_before(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRoundedRectangle(x, y, this->width, this->height, this->radius, this->radius, this->background);
}

void Keyboard::draw_cell(CanvasDrawingSession^ ds, VirtualKey key, bool focused, bool tapped, float x, float y, float width, float height) {
	auto label = this->key_label(key);

	if (focused) {
		auto highbrush = (tapped ? this->taplight : this->highlight);

		ds->FillRoundedRectangle(x, y, width, height, this->radius, this->radius, highbrush);
	}

	ds->DrawRoundedRectangle(x, y, width, height, this->radius, this->radius, this->border, 2.0F);

	if (label != nullptr) {
		Rect box = label->LayoutBounds;
		float tx = x + (width - box.Width) * 0.5F;
		float ty = y + (height - box.Height) * 0.5F;

		ds->DrawTextLayout(label, tx, ty, this->foreground);
	}
}

/*************************************************************************************************/
void Keyboard::on_hover(float local_x, float local_y) {
	if (!this->tapped) {
		this->current_key = this->find_tapped_key(local_x, local_y);
		this->master->notify_graphlet_updated(this);
	}
}

void Keyboard::on_tap(float local_x, float local_y) {
	VirtualKey found = this->find_tapped_key(local_x, local_y);

	if (found != VirtualKey::None) {
		this->master->on_key(found, true);
	}
}

bool Keyboard::on_character(unsigned int key) {
	bool handled = false;
	VirtualKey received = this->find_received_key(key);

	if (received != VirtualKey::None) {
		handled = this->master->on_key(received, true);
	}

	return handled;
}

bool Keyboard::on_key(VirtualKey key, bool wargrey_keyboard) {
	this->current_key = key;
	this->taptime = this->uptime;
	this->tapped = true;
	this->master->notify_graphlet_updated(this);

	return false;
}

void Keyboard::on_goodbye(float local_x, float local_y) {
	this->current_key = VirtualKey::None;
	this->master->notify_graphlet_updated(this);
}

VirtualKey Keyboard::find_tapped_key(float mouse_x, float mouse_y) {
	VirtualKey found = VirtualKey::None;

	for (unsigned char i = 0; i < this->keynum; i++) {
		Rect box = this->cell_boxes[i];
		float cx = box.X;
		float cy = box.Y;
		float cwidth = box.Width;
		float cheight = box.Height;

		if ((cx < mouse_x) && (mouse_x < cx + cwidth) && (cy < mouse_y) && (mouse_y < cy + cheight)) {
			found = this->cells[i].key;
		}
	}

	return found;
}

VirtualKey Keyboard::find_received_key(unsigned int keycode) {
	return VirtualKey::None;
}

/*************************************************************************************************/
void Keyboard::fill_auto_position(float* x, float* y, IGraphlet* g, GraphletAnchor a) {
	float Width = this->master->actual_width();
	float Height = this->master->actual_height();
	float width, height;

	this->fill_extent(0.0F, 0.0F, &width, &height);

	if (g == nullptr) {
		SET_BOX(x, Width - width);
		SET_BOX(y, Height - height);
	} else {
		float x0, y0, lx, ty;

		this->master->fill_graphlet_location(g, &x0, &y0, a);
		this->master->fill_graphlet_location(g, &lx, &ty, GraphletAnchor::LT);

		if (x0 + width > Width) {
			x0 = lx - width;
		}
		
		if (y0 + height > Height) {
			y0 = ty - height;
		}

		SET_VALUES(x, x0, y, y0);
	}
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
	this->master->notify_graphlet_updated(this);
}

bool IKeyboard::shown() {
	return this->_shown;
}
