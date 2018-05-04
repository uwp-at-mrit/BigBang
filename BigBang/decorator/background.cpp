﻿#include "decorator/background.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

BackgroundDecorator::BackgroundDecorator(unsigned int hex, float tinset, float rinset, float binset, float linset) {
	this->color = Colours::make(hex);
	this->top_inset = tinset;
	this->right_inset = rinset;
	this->bottom_inset = binset;
	this->left_inset = linset;
}

BackgroundDecorator::BackgroundDecorator(unsigned int color, float inset)
	: BackgroundDecorator(color, inset, inset, inset, inset) {}

BackgroundDecorator::BackgroundDecorator(unsigned int color, float hinset, float vinset)
	: BackgroundDecorator(color, vinset, hinset, vinset, hinset) {}

BackgroundDecorator::~BackgroundDecorator() {}

void BackgroundDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	float width = Width - this->left_inset - this->right_inset;
	float height = Height - this->top_inset - this->bottom_inset;
	
	ds->FillRectangle(this->left_inset, this->top_inset, width, height, this->color);
}