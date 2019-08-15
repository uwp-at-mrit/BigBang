#include <map>

#include "graphlet/symbol/dig/vectorlet.hpp"

#include "datum/string.hpp"

#include "colorspace.hpp"
#include "paint.hpp"
#include "text.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static std::map<long long, CanvasSolidColorBrush^> vector_colors;

static CanvasStrokeStyle^ vector_strokes[] = {
	make_dash_stroke(CanvasDashStyle::Solid),
	make_dash_stroke(CanvasDashStyle::Dash),
	make_dash_stroke(CanvasDashStyle::Dot),
	make_dash_stroke(CanvasDashStyle::DashDot),
	make_dash_stroke(CanvasDashStyle::DashDotDot)
};

static CanvasSolidColorBrush^ vector_colors_ref(long long gbr) {
	auto maybe_color = vector_colors.find(gbr);
	CanvasSolidColorBrush^ color = nullptr;

	if (maybe_color == vector_colors.end()) {
		if (gbr == 0LL) {
			color = Colours::Foreground;
		} else {
			color = make_solid_brush(gbra((unsigned int)gbr));
		}

		vector_colors.insert(std::pair<long long, CanvasSolidColorBrush^>(gbr, color));
	} else {
		color = maybe_color->second;
	}

	return color;
}

/*************************************************************************************************/
DigVectorlet::DigVectorlet(DigVectorMap^ map) : map(map), xscale(1.0), yscale(1.0) {
	this->enable_resizing(true);
}

void DigVectorlet::construct() {
	this->map->fill_enclosing_box(&this->x, &this->y, &this->width, &this->height);
}

void DigVectorlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, float(this->width), height, float(this->height));
}

void DigVectorlet::resize(float width, float height) {
	if ((this->width != width) || (this->height != height)) {
		double geox, geoy, geowidth, geoheight;

		this->map->fill_enclosing_box(&geox, &geoy, &geowidth, &geoheight);

		this->xscale = double(width) / geowidth;
		this->yscale = double(height) / geoheight;

		this->x = geox * this->xscale;
		this->y = geoy * this->yscale;
		this->width = width;
		this->height = height;

		this->notify_updated();
	}
}

void DigVectorlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	IDigDatum* datum = nullptr;
	float ds_rx = x + Width;
	float ds_by = y + Height;

	this->map->step();

	while ((datum = this->map->step()) != nullptr) {
		switch (datum->type) {
		case DigDatumType::Line: {
			LineDig* l = static_cast<LineDig*>(datum);
			float x0 = float(l->x * this->xscale - this->x);
			float y0 = float(l->y * this->yscale - this->y);
			float x1 = float(l->stop_x * this->xscale - this->x);
			float y1 = float(l->stop_y * this->yscale - this->y);
			float lwidth = ((l->linewidth > 0) ? float(l->linewidth) : 1.0F);

			if (((x0 <= ds_rx) || (x1 >= x)) && ((y0 <= ds_by) || (y1 >= y))) {
				ds->DrawLine(x0, y0, x1, y1, vector_colors_ref(l->color), lwidth, vector_strokes[l->style]);
			} else {
				//this->get_logger()->log_message(Log::Info, L"(%f, %f, %f, %f)", x0, y0, x1, y1);
			}
		}; break;
		case DigDatumType::Circle: {
			CircleDig* c = static_cast<CircleDig*>(datum);
			float cx = float(c->x * this->xscale - this->x);
			float cy = float(c->y * this->yscale - this->y);
			float rx = float(c->radius * this->xscale);
			float ry = float(c->radius * this->yscale);

			if ((cx >= (x - rx)) && (cx <= (ds_rx + rx)) && (cy >= (y - ry)) && (cy <= (ds_by + ry))) {
				CanvasStrokeStyle^ border_style = vector_strokes[c->style];
				ICanvasBrush^ border_color = vector_colors_ref(c->color);

				if (c->filled) {
					ds->FillEllipse(cx, cy, rx, ry, vector_colors_ref(c->fill_color));
				}

				ds->DrawEllipse(cx, cy, rx, ry, border_color, 1.0F, border_style);
			}
		}; break;
		}
	}

	// this->get_logger()->log_message(Log::Info, L"BOX: (%f, %f, %f, %f)", x, y, rx, by);
}
