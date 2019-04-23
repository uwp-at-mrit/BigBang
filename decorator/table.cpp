#pragma once

#include "decorator/table.hpp"
#include "brushes.hxx"

#include "datum/box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

ITableDecorator::ITableDecorator(ICanvasBrush^ color, size_t count, float radius)
	: count((unsigned int)(count)), radius(radius), color(color) {}

unsigned int ITableDecorator::cell_count() {
	return this->count;
}

int ITableDecorator::find_cell(float mx, float my) {
	float x, y, width, height;
	int cell_idx = -1;

	for (unsigned int i = 0; i < this->count; i++) {
		this->fill_cell_extent(i, &x, &y, &width, &height);

		if ((x <= mx) && (mx <= (x + width)) && (y <= my) && (my <= (y + height))) {
			cell_idx = int(i);
			break;
		}
	}

	return cell_idx;
}

void ITableDecorator::fill_cell_anchor(unsigned int idx, float fx, float fy, float* x, float* y) {
	float x0, y0, width, height;

	this->fill_cell_extent(idx, &x0, &y0, &width, &height);

	SET_BOX(x, x0 + width * fx);
	SET_BOX(y, y0 + height * fy);
}

void ITableDecorator::draw_before(CanvasDrawingSession^ ds, float Width, float Height) {
	float x, y, width, height;

	for (unsigned int i = 0; i < this->count; i++) {
		this->fill_cell_extent(i, &x, &y, &width, &height);
		this->draw_cell(ds, x, y, width, height, this->radius, this->color);
	}
}

void ITableDecorator::draw_cell(CanvasDrawingSession^ ds, float x, float y, float width, float height, float radius, ICanvasBrush^ color) {
	ds->FillRoundedRectangle(x, y, width, height, radius, radius, color);
}

/*************************************************************************************************/
TableDecorator::TableDecorator(unsigned int color, size_t count, size_t col, float hgap, float vgap, float radius)
	: TableDecorator(Colours::make(color), count, col, hgap, vgap, radius) {
}

TableDecorator::TableDecorator(ICanvasBrush^ color, size_t count, size_t col, float hgap, float vgap, float radius)
	: ITableDecorator(color, count, radius), col(col) {
	this->row = count / col + ((count % col == 0) ? 0 : 1);
	this->hgapsize = (hgap < 0.0F) ? 2.0F : hgap;
	this->vgapsize = (vgap < 0.0F) ? this->hgapsize : vgap;
}

void TableDecorator::fill_cell_extent(unsigned int idx, float* x, float* y, float* width, float* height) {
	float cell_width = (this->actual_width() - this->hgapsize) / float(col) - this->hgapsize;
	float cell_height = (this->actual_height() - this->vgapsize) / float(row) - this->vgapsize;
	float cell_x = (this->hgapsize + cell_width) * float(idx % col) + this->hgapsize;
	float cell_y = (this->vgapsize + cell_height) * float(idx / col) + this->vgapsize;

	SET_VALUES(x, cell_x, y, cell_y);
	SET_VALUES(width, cell_width, height, cell_height);
}

/*************************************************************************************************/
CellDecorator::CellDecorator(unsigned int color, const Rect* src, size_t count, float radius)
	: CellDecorator(Colours::make(color), src, count, radius) {}

CellDecorator::CellDecorator(ICanvasBrush^ color, const Rect* src, size_t count, float radius)
	: ITableDecorator(color, count, radius) {
	this->boxes = new Rect[count];
	memcpy(this->boxes, src, sizeof(Rect) * count);
}

CellDecorator::~CellDecorator() {
	if (this->boxes != nullptr) {
		delete[] this->boxes;
	}
}

void CellDecorator::fill_cell_extent(unsigned int idx, float* x, float* y, float* width, float* height) {
	if (idx < this->cell_count()) {
		float W = this->actual_width();
		float H = this->actual_height();
		float x0 = this->boxes[idx].X;
		float y0 = this->boxes[idx].Y;
		float w0 = this->boxes[idx].Width;
		float h0 = this->boxes[idx].Height;

		/** Note
		 * This design is not elegant per se, maybe a new ITableDecorator should be involved.
		 * Meanwhile, it is a tradeoff, since rectangles with size 1x1 or located out of screen
		 * are almost useless.
		 */

		SET_BOX(x, ((x0 < 1.0F) ? (x0 * W) : x0));
		SET_BOX(y, ((y0 < 1.0F) ? (y0 * H) : y0));
		SET_BOX(width,  ((w0 <= 1.0F) ? (w0 * W) : w0));
		SET_BOX(height, ((h0 <= 1.0F) ? (h0 * H) : h0));
	}
}
