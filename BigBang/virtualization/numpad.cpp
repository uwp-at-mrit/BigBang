#include "virtualization/numpad.hpp"

#include "planet.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "syslog.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

private enum NumpadCell { Col = 0, Row, NCol, NRow };

static unsigned int NUMPAD_KEYNUM = 14;
static long long numpad_tap_duration = 3000000LL;

static Platform::String^ labels[] = { "9", "8", "7", "6", "5", "4", "3", "2", "1", ".", "0", "B", "E", "R" };
static const char lmetas[][4] = {
	{ 2, 0, 1, 1 }, { 1, 0, 1, 1 }, { 0, 0, 1, 1 },
    { 2, 1, 1, 1 }, { 1, 1, 1, 1 }, { 0, 1, 1, 1 },
    { 2, 2, 1, 1 }, { 1, 2, 1, 1 }, { 0, 2, 1, 1 },
    { 2, 3, 1, 1 }, { 0, 3, 2, 1 },

    { 3, 0, 1, 1 }, { 3, 1, 1, 1 }, { 3, 2, 1, 2 }
};

static void numpad_fill_cell(KeyboardCell* cell, int idx, float cellsize, float gapsize, float em, float ch) {
	char col = lmetas[idx][NumpadCell::Col];
	char row = lmetas[idx][NumpadCell::Row];
	char ncol = lmetas[idx][NumpadCell::NCol];
	char nrow = lmetas[idx][NumpadCell::NRow];
	float cwidth = cellsize * float(ncol) + gapsize * float(ncol - 1);
	float cheight = cellsize * float(nrow) + gapsize * float(nrow - 1);

	cell->x = gapsize * float(col + 1) + cellsize * float(col);
	cell->y = gapsize * float(row + 1) + cellsize * float(row);
	cell->width = cwidth;
	cell->height = cheight;
	cell->label_x = (cwidth - ch) * 0.5F;
	cell->label_y = (cheight - em) * 0.5F;
}

/*************************************************************************************************/
Numpad::Numpad(IPlanet* master, float fontsize) : IKeyboard(master, NUMPAD_KEYNUM) {
	this->label_font = make_text_format("Consolas", fontsize);
	this->enable_events(true);
	this->show(false);
	this->on_goodbye();
}

void Numpad::create() {
	syslog(Log::Info, "create");
	TextExtent ts = get_text_extent("0", this->label_font);
	Color fg = system_foreground_brush()->Color;
	Color bg = system_background_brush()->Color;
	
	this->foreground = system_foreground_brush();
	this->background = make_solid_brush(rgba(bg, 0.8));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));

	this->em = ts.height;
	this->gapsize = this->em * 0.382F;
	this->cellsize = this->em * 1.618F;
	this->radius = this->gapsize * 0.5F;
}

void Numpad::fill_cell(KeyboardCell* cell, unsigned int idx) {
	numpad_fill_cell(cell, idx, this->cellsize, this->gapsize, this->em, this->em * 0.5F);
}

void Numpad::fill_extent(float x, float y, float* w, float* h) {
	float size = this->cellsize * 4.0F + this->gapsize * 5.0F;

	SET_VALUES(w, size, h, size);
}

void Numpad::update(long long count, long long interval, long long uptime, bool is_slow) {
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

void Numpad::on_hover(float local_x, float local_y, bool shifted, bool controled) {
	if (!this->tapped) {
		this->current_cell = this->find_cell(local_x, local_y);
	}
}

void Numpad::on_tap(float local_x, float local_y, bool shifted, bool controled) {
	this->current_cell = this->find_cell(local_x, local_y);
	this->taptime = this->uptime;
	this->tapped = true;
}

void Numpad::on_goodbye() {
	this->current_cell = -1;
}

void Numpad::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRoundedRectangle(x, y, Width, Height, this->radius, this->radius, this->background);
		
	for (unsigned char i = 0; i < NUMPAD_KEYNUM; i++) {
		float cx = x + this->cells[i].x;
		float cy = y + this->cells[i].y;
		float cwidth = this->cells[i].width;
		float cheight = this->cells[i].height;
		float tx = cx + this->cells[i].label_x;
		float ty = cy + this->cells[i].label_y;

		if (this->current_cell == i) {
			auto highbrush = (this->tapped ? this->taplight : this->highlight);
			ds->FillRoundedRectangle(cx, cy, cwidth, cheight, this->radius, this->radius, highbrush);
		}

		ds->DrawRoundedRectangle(cx, cy, cwidth, cheight, this->radius, this->radius, this->border, 2.0F);
		ds->DrawText(labels[i], tx, ty, this->foreground, this->label_font);
	}
}

void Numpad::fill_auto_position(float* x, float* y) {
	IPlanet* planet = this->master;
	ISnip* target_snip = planet->get_focus_snip();

	if (target_snip == nullptr) {
		float Width = planet->info->master->actual_width;
		float Height = planet->info->master->actual_height;
		float this_width, this_height;

		this->fill_extent(0.0F, 0.0F, &this_width, &this_height);
		SET_BOX(x, Width - this_width);
		SET_BOX(y, Height - this_height);
	} else {
		planet->fill_snip_location(target_snip, x, y, SnipCenterPoint::LB);
	}
}
