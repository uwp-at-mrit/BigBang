#pragma once

#include <algorithm>

#include "decorator/cell.hpp"
#include "configuration.hpp"
#include "brushes.hxx"
#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

Rect WarGrey::SCADA::make_raw_cell(float x, float y, float width, float height) {
	return Rect(x, y, width, height);
}

Rect WarGrey::SCADA::make_fit_cell(float x, float y, float width, float height) {
	return Rect(application_fit_size(x), application_fit_size(y),
		application_fit_size(width), application_fit_size(height));
}

/*************************************************************************************************/
CellDecorator::CellDecorator(unsigned int color, const Rect* src, size_t count, float radius)
	: CellDecorator(Colours::make(color), src, count, radius) {}

CellDecorator::CellDecorator(ICanvasBrush^ color, const Rect* src, size_t count, float radius)
	: color(color), count(count), radius(radius) {
	this->boxes = new Rect[count];
	for (size_t i = 0; i < count; i++) {
		this->boxes[i] = src[i];
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

void CellDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (size_t i = 0; i < count; i++) {
		ds->FillRoundedRectangle(this->boxes[i].X, this->boxes[i].Y,
			this->boxes[i].Width, this->boxes[i].Height,
			this->radius, this->radius,
			this->color);
	}
}
