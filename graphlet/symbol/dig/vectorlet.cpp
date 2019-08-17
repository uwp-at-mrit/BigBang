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

#define StrokeWidth(sw) ((sw > 0) ? float(sw) : 1.0F)

static CanvasStrokeStyle^ vector_strokes[] = {
	make_dash_stroke(CanvasDashStyle::Solid),
	make_dash_stroke(CanvasDashStyle::Dash),
	make_dash_stroke(CanvasDashStyle::Dot),
	make_dash_stroke(CanvasDashStyle::DashDot),
	make_dash_stroke(CanvasDashStyle::DashDotDot)
};

static CanvasSolidColorBrush^ vector_colors_ref(long long bgr) {
	auto maybe_color = vector_colors.find(bgr);
	CanvasSolidColorBrush^ color = nullptr;

	if (maybe_color == vector_colors.end()) {
		if (bgr == 0LL) {
			color = Colours::Foreground;
		} else {
			color = make_solid_brush(gbra((unsigned int)bgr));
		}

		vector_colors.insert(std::pair<long long, CanvasSolidColorBrush^>(bgr, color));
	} else {
		color = maybe_color->second;
	}

	return color;
}

/*************************************************************************************************/
DigVectorlet::DigVectorlet(DigVectorMap^ map, double width, double height, double tx, double ty)
	: map(map), initial_width(width), initial_height(height), xscale(1.0), yscale(1.0), xtranslation(tx), ytranslation(ty) {
	this->enable_resizing(true);
}

void DigVectorlet::construct() {
	this->width = float(this->initial_width * this->xscale);
	this->height = float(this->initial_height * this->yscale);
}

void DigVectorlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void DigVectorlet::resize(float new_width, float new_height) {
	if ((this->width != new_width) || (this->height != new_height)) {
		this->xscale = double(new_width / this->initial_width);
		this->yscale = double(new_height / this->initial_height);

		this->width = new_width;
		this->height = new_height;

		this->notify_updated();
	}
}

void DigVectorlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	IDigDatum* datum = nullptr;
	float ds_rx = x + Width;
	float ds_by = y + Height;

	this->map->rewind();

	while ((datum = this->map->step()) != nullptr) {
		switch (datum->type) {
		case DigDatumType::Line: {
			LineDig* l = static_cast<LineDig*>(datum);
			float y0 = Height - float((l->x + this->ytranslation) * this->xscale) + y;
			float x0 = float((l->y + this->xtranslation) * this->yscale) + x;
			float y1 = Height - float((l->stop_x + this->ytranslation) * this->xscale) + y;
			float x1 = float((l->stop_y + this->xtranslation) * this->yscale) + x;
			float lwidth = StrokeWidth(l->linewidth);

#ifdef _DEBUG
			this->get_logger()->log_message(Log::Info, L"Line: (%f, %f, %f, %f)", x0, y0, x1, y1);
#endif

			//if (((x0 <= ds_rx) || (x1 >= x)) && ((y0 <= ds_by) || (y1 >= y))) {
				ds->DrawLine(x0, y0, x1, y1, vector_colors_ref(l->color), lwidth, vector_strokes[l->style]);
			//}
		}; break;
		case DigDatumType::Circle: {
			CircleDig* c = static_cast<CircleDig*>(datum);
			float cy = Height - float((c->x + this->ytranslation) * this->xscale) + y;
			float cx = float((c->y + this->xtranslation) * this->yscale) + x;
			float rx = float(c->radius * this->xscale);
			float ry = float(c->radius * this->yscale);

#ifdef _DEBUG
			this->get_logger()->log_message(Log::Info, L"Circle: (%f, %f, %f, %f)", cx, cy, rx, ry);
#endif

			//if ((cx >= (x - rx)) && (cx <= (ds_rx + rx)) && (cy >= (y - ry)) && (cy <= (ds_by + ry))) {
				CanvasStrokeStyle^ border_style = vector_strokes[c->style];
				ICanvasBrush^ border_color = vector_colors_ref(c->color);

				if (c->filled) {
					ds->FillEllipse(cx, cy, rx, ry, vector_colors_ref(c->fill_color));
				}

				ds->DrawEllipse(cx, cy, rx, ry, border_color, 1.0F, border_style);
			//}
		}; break;
		default: {
			this->get_logger()->log_message(Log::Info, datum->type.ToString());
		}
		}
	}

	this->get_logger()->log_message(Log::Info, L"Region: (%f, %f, %f, %f)", x, y, Width, Height);
}
