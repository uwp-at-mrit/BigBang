#include "numpadlet.hpp"

#include "planet.hpp"
#include "paint.hpp"
#include "text.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Numpadlet::Numpadlet(float fontsize) {
	this->label_font = make_text_format("Consolas", fontsize);
}

void Numpadlet::construct() {
	TextExtent ts = get_text_extent("0", this->label_font);

	this->em = ts.height;
	this->gapsize = this->em * 0.382F;
	this->cellsize = this->em * 1.618F;
	this->radius = this->gapsize * 0.5F;
}

void Numpadlet::fill_extent(float x, float y, float* w, float* h) {
	if (this->has_focus()) {
		float size = this->cellsize * 4.0F + this->gapsize * 5.0F;

		SET_VALUES(w, size , h, size);
	} else {
		SET_VALUES(w, 0.0F, h, 0.0F);
	}
}

void Numpadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->has_focus()) {
		ds->FillRoundedRectangle(x, y, Width, Height, this->radius, this->radius, system_background_brush());
		this->draw_cell(ds, x, y, "9", 2, 0, 1, 1);
		this->draw_cell(ds, x, y, "8", 1, 0, 1, 1);
		this->draw_cell(ds, x, y, "7", 0, 0, 1, 1);
		this->draw_cell(ds, x, y, "6", 2, 1, 1, 1);
		this->draw_cell(ds, x, y, "5", 1, 1, 1, 1);
		this->draw_cell(ds, x, y, "4", 0, 1, 1, 1);
		this->draw_cell(ds, x, y, "3", 2, 2, 1, 1);
		this->draw_cell(ds, x, y, "2", 1, 2, 1, 1);
		this->draw_cell(ds, x, y, "1", 0, 2, 1, 1);
		this->draw_cell(ds, x, y, ".", 2, 3, 1, 1);
		this->draw_cell(ds, x, y, "0", 0, 3, 2, 1);

		this->draw_cell(ds, x, y, "B", 3, 0, 1, 1);
		this->draw_cell(ds, x, y, "E", 3, 1, 1, 1);
		this->draw_cell(ds, x, y, "R", 3, 2, 1, 2);
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

void Numpadlet::draw_cell(CanvasDrawingSession^ ds, float x, float y, Platform::String^ label, int col, int row, int ncol, int nrow) {
	float cx = x + this->gapsize * (col + 1) + this->cellsize * col;
	float cy = y + this->gapsize * (row + 1) + this->cellsize * row;
	float cwidth = this->cellsize * ncol + this->gapsize * (ncol - 1);
	float cheight = this->cellsize * nrow + this->gapsize * (nrow - 1);
	float tx = cx + (cwidth - this->em * 0.5F) * 0.5F;
	float ty = cy + (cheight - this->em) * 0.5F;

	ds->DrawRoundedRectangle(cx, cy, cwidth, cheight, this->radius, this->radius, system_highlight_brush(), 2.0F);
	ds->DrawText(label, tx, ty, system_foreground_brush(), this->label_font);
}
