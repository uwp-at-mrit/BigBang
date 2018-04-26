#pragma once

#include <algorithm>

#include "decorator/cell.hpp"
#include "configuration.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

CellDecorator::CellDecorator(unsigned int color, const Rect* src, size_t count, float radius)
	: CellDecorator(Colours::make(color), src, count, radius) {}

CellDecorator::CellDecorator(ICanvasBrush^ color, const Rect* src, size_t count, float radius)
	: color(color), count(count), radius(radius) {
	this->boxes = new Rect[count];
	for (size_t i = 0; i < count; i++) {
		this->boxes[i].X = application_fit_size(src[i].X);
		this->boxes[i].Y = application_fit_size(src[i].Y);
		this->boxes[i].Width = application_fit_size(src[i].Width);
		this->boxes[i].Height = application_fit_size(src[i].Height);
	}
}

CellDecorator::~CellDecorator() {
	if (this->boxes != nullptr) {
		delete[] this->boxes;
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
