#include "numpadlet.hpp"

#include "planet.hpp"
#include "paint.hpp"
#include "text.hpp"
#include "syslog.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

private enum Numpad { Col = 0, Row, NCol, NRow };
private enum NumpadCell { X = 0, Y, Width, Height, Xoff, Yoff };

static Platform::String^ labels[] = { "9", "8", "7", "6", "5", "4", "3", "2", "1", ".", "0", "B", "E", "R" };
static const char lmetas[][4] = {
	{ 2, 0, 1, 1 }, { 1, 0, 1, 1 }, { 0, 0, 1, 1 },
    { 2, 1, 1, 1 }, { 1, 1, 1, 1 }, { 0, 1, 1, 1 },
    { 2, 2, 1, 1 }, { 1, 2, 1, 1 }, { 0, 2, 1, 1 },
    { 2, 3, 1, 1 }, { 0, 3, 2, 1 },

    { 3, 0, 1, 1 }, { 3, 1, 1, 1 }, { 3, 2, 1, 2 }
};

static void numpad_fill_cell(float* cell, int idx, float cellsize, float gapsize, float em, float ch) {
	char col = lmetas[idx][Numpad::Col];
	char row = lmetas[idx][Numpad::Row];
	char ncol = lmetas[idx][Numpad::NCol];
	char nrow = lmetas[idx][Numpad::NRow];
	float cwidth = cellsize * float(ncol) + gapsize * float(ncol - 1);
	float cheight = cellsize * float(nrow) + gapsize * float(nrow - 1);

	cell[NumpadCell::X] = gapsize * float(col + 1) + cellsize * float(col);
	cell[NumpadCell::Y] = gapsize * float(row + 1) + cellsize * float(row);
	cell[NumpadCell::Width] = cwidth;
	cell[NumpadCell::Height] = cheight;
	cell[NumpadCell::Xoff] = (cwidth - ch) * 0.5F;
	cell[NumpadCell::Yoff] = (cheight - em) * 0.5F;
}

/*************************************************************************************************/
Numpadlet::Numpadlet(float fontsize) {
	this->label_font = make_text_format("Consolas", fontsize);
	this->enable_events(true);
	this->on_goodbye();
}

void Numpadlet::construct() {
	TextExtent ts = get_text_extent("0", this->label_font);
	Color fg = system_foreground_brush()->Color;
	Color bg = system_background_brush()->Color;
	
	this->foreground = system_foreground_brush();
	this->background = make_solid_brush(rgba(bg, 0.618));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));

	this->em = ts.height;
	this->gapsize = this->em * 0.382F;
	this->cellsize = this->em * 1.618F;
	this->radius = this->gapsize * 0.5F;
	this->fill_cells();
}

void Numpadlet::fill_extent(float x, float y, float* w, float* h) {
	if (this->has_focus()) {
		float size = this->cellsize * 4.0F + this->gapsize * 5.0F;

		SET_VALUES(w, size , h, size);
	} else {
		SET_VALUES(w, 0.0F, h, 0.0F);
	}
}

void Numpadlet::update(long long count, long long interval, long long uptime, bool is_slow) {
	this->uptime = uptime;

	if (this->tapped) {
		if (uptime - this->taptime > 2000000) {
			this->tapped = false;
		}
	}
}

void Numpadlet::on_hover(float local_x, float local_y, bool shifted, bool controled) {
	if (!this->tapped) {
		this->current_cell = this->find_cell(local_x, local_y);
	}
}

void Numpadlet::on_tap(float local_x, float local_y, bool shifted, bool controled) {
	this->current_cell = this->find_cell(local_x, local_y);
	this->taptime = this->uptime;
	this->tapped = true;
}

void Numpadlet::on_goodbye() {
	this->current_cell = -1;
}

void Numpadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->has_focus()) {
		ds->FillRoundedRectangle(x, y, Width, Height, this->radius, this->radius, this->background);
		
		for (char i = 0; i < NUMPAD_KEYNUM; i++) {
			float cx = x + this->cells[i][NumpadCell::X];
			float cy = y + this->cells[i][NumpadCell::Y];
			float cwidth = this->cells[i][NumpadCell::Width];
			float cheight = this->cells[i][NumpadCell::Height];
			float tx = cx + this->cells[i][NumpadCell::Xoff];
			float ty = cy + this->cells[i][NumpadCell::Yoff];

			if (this->current_cell == i) {
				auto highbrush = (this->tapped ? this->taplight : this->highlight);
				ds->FillRoundedRectangle(cx, cy, cwidth, cheight, this->radius, this->radius, highbrush);
			}

			ds->DrawRoundedRectangle(cx, cy, cwidth, cheight, this->radius, this->radius, this->border, 2.0F);
			ds->DrawText(labels[i], tx, ty, this->foreground, this->label_font);
		}
	}
}

void Numpadlet::show(ISnip* target_snip, float xoff, float yoff) {
	if (this->info != nullptr) {
		float snip_x, snip_y;
		IPlanet* planet = this->info->master;

		planet->fill_snip_location(target_snip, &snip_x, &snip_y, SnipCenterPoint::LB);
		planet->move_to(this, snip_x + xoff, snip_y + yoff);
		planet->set_caret_owner(this);
	}
}

bool Numpadlet::has_focus() {
	return ((this->info != nullptr) && (this->info->master->get_focus_snip() == this));
}

void Numpadlet::fill_cells() {
	float ch = this->em * 0.5F;

	for (char i = 0; i < NUMPAD_KEYNUM; i++) {
		numpad_fill_cell(this->cells[i], i, this->cellsize, this->gapsize, this->em, ch);
	}
}

int Numpadlet::find_cell(float mouse_x, float mouse_y) {
	int found = -1;

	for (char i = 0; i < NUMPAD_KEYNUM; i++) {
		float cx = this->cells[i][NumpadCell::X];
		float cy = this->cells[i][NumpadCell::Y];
		float cwidth = this->cells[i][NumpadCell::Width];
		float cheight = this->cells[i][NumpadCell::Height];

		if ((cx < mouse_x) && (mouse_x < cx + cwidth) && (cy < mouse_y) && (mouse_y < cy + cheight)) {
			found = i;
		}
	}

	return found;
}
