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

CellDecorator::CellDecorator(unsigned int color, float width, float height, size_t count, size_t col, float gapsize, float radius)
	: CellDecorator(Colours::make(color), width, height, count, col, gapsize, radius) {
}

CellDecorator::CellDecorator(ICanvasBrush^ color, float width, float height, size_t count, size_t col, float gapsize, float radius)
	: color(color), count(count), radius(radius) {
	size_t row = count / col + ((count % col == 0) ? 0 : 1);
	float cell_width = (width - gapsize) / float(col) - gapsize;
	float cell_height = (height - gapsize) / float(row) - gapsize;

	this->boxes = new Rect[count];
	for (unsigned int i = 0; i < count; i++) {
		this->boxes[i].X = (gapsize + cell_width) * float(i % col) + gapsize;
		this->boxes[i].Y = (gapsize + cell_height) * float(i / col) + gapsize;
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

void CellDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (size_t i = 0; i < count; i++) {
		ds->FillRoundedRectangle(this->boxes[i].X, this->boxes[i].Y,
			this->boxes[i].Width, this->boxes[i].Height,
			this->radius, this->radius,
			this->color);
	}
}
