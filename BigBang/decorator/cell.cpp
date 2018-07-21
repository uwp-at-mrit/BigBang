#pragma once

#include <algorithm>

#include "decorator/cell.hpp"
#include "brushes.hxx"
#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
CellDecorator::CellDecorator(unsigned int color, const Rect* src, size_t count, float radius)
	: CellDecorator(Colours::make(color), src, count, radius) {}

CellDecorator::CellDecorator(ICanvasBrush^ color, const Rect* src, size_t count, float radius)
	: color(color), count(count), radius(radius) {
	this->boxes = new Rect[count];
	memcpy(this->boxes, src, sizeof(Rect) * count);
}

CellDecorator::CellDecorator(unsigned int color, float width, float height, size_t count, size_t col, float hgap, float vgap, float radius)
	: CellDecorator(Colours::make(color), width, height, count, col, hgap, vgap, radius) {}

CellDecorator::CellDecorator(ICanvasBrush^ color, float width, float height, size_t count, size_t col, float hgap, float vgap, float radius)
	: color(color), count(count), radius(radius) {
	size_t row = count / col + ((count % col == 0) ? 0 : 1);
	float hgapsize = (hgap < 0.0F) ? 2.0F : hgap;
	float vgapsize = (vgap < 0.0F) ? hgapsize : vgap;
	float cell_width = (width - hgapsize) / float(col) - hgapsize;
	float cell_height = (height - vgapsize) / float(row) - vgapsize;

	this->boxes = new Rect[count];
	for (unsigned int i = 0; i < count; i++) {
		this->boxes[i].X = (hgapsize + cell_width) * float(i % col) + hgapsize;
		this->boxes[i].Y = (vgapsize + cell_height) * float(i / col) + vgapsize;
		this->boxes[i].Width = cell_width;
		this->boxes[i].Height = cell_height;
	}
}

CellDecorator::~CellDecorator() {
	if (this->boxes != nullptr) {
		delete[] this->boxes;
	}
}

void CellDecorator::fill_cell_extent(unsigned int idx, float* x, float* y, float* width, float* height) {
	if (idx < this->count) {
		SET_BOX(x, this->boxes[idx].X);
		SET_BOX(y, this->boxes[idx].Y);
		SET_BOX(width, this->boxes[idx].Width);
		SET_BOX(height, this->boxes[idx].Height);
	}
}

void CellDecorator::fill_cell_anchor(unsigned int idx, float fx, float fy, float* x, float* y) {
	float x0, y0, width, height;

	this->fill_cell_extent(idx, &x0, &y0, &width, &height);

	SET_BOX(x, x0 + width * fx);
	SET_BOX(y, y0 + height * fy);
}

int CellDecorator::find_cell(float mx, float my) {
	float x, y, width, height;
	int cell_idx = -1;

	for (size_t i = 0; i < this->count; i++) {
		this->fill_cell_extent((unsigned int)i, &x, &y, &width, &height);

		if ((x <= mx) && (mx <= (x + width)) && (y <= my) && (my <= (y + height))) {
			cell_idx = int(i);
			break;
		}
	}

	return cell_idx;
}

void CellDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (size_t i = 0; i < this->count; i++) {
		ds->FillRoundedRectangle(this->boxes[i].X, this->boxes[i].Y,
			this->boxes[i].Width, this->boxes[i].Height,
			this->radius, this->radius,
			this->color);

		ds->DrawRoundedRectangle(this->boxes[i].X, this->boxes[i].Y,
			this->boxes[i].Width, this->boxes[i].Height,
			this->radius, this->radius,
			Colours::Firebrick);
	}
}
