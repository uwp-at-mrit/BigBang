#include <map>

#include "graphlet/filesystem/project/depthlet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"

#include "colorspace.hpp"
#include "geometry.hpp"
#include "brushes.hxx"
#include "paint.hpp"
#include "shape.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
Depthlet::Depthlet(XyzDoc^ depths, double width, double height) : depths(depths), width(float(width)), height(float(height)) {
	this->enable_resizing(false);
	this->enable_events(false, false);
	this->camouflage(true);
}

void Depthlet::construct() {
	this->plainfont = make_text_format();
}

void Depthlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void Depthlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	IDigDatum* dig = nullptr;
	float ds_rx = x + Width;
	float ds_by = y + Height;
}
