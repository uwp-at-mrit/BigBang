#include <map>

#include "graphlet/symbol/dig/digmaplet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"

#include "colorspace.hpp"
#include "brushes.hxx"
#include "paint.hpp"
#include "shape.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

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

static CanvasStrokeStyle^ vector_stroke_ref(long long idx) {
	CanvasStrokeStyle^ style = vector_strokes[0];

	if (idx < sizeof(vector_strokes) / sizeof(CanvasStrokeStyle^)) {
		style = vector_strokes[idx];
	} 

	return style;
}

static inline const wchar_t* visible_mark(bool visible) {
	Platform::String^ desc = (visible ? "" : " [invisible]");

	return desc->Data();
}

/*************************************************************************************************/
DigMaplet::DigMaplet(DigMap^ map, double width, double height, double tx, double ty)
	: map(map), initial_width(width), initial_height(height), xscale(1.0), yscale(1.0), xtranslation(tx), ytranslation(ty) {
	this->enable_resizing(true);
}

void DigMaplet::construct() {
	this->width = float(this->initial_width * this->xscale);
	this->height = float(this->initial_height * this->yscale);
}

void DigMaplet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void DigMaplet::resize(float new_width, float new_height) {
	if ((this->width != new_width) || (this->height != new_height)) {
		this->xscale = double(new_width / this->initial_width);
		this->yscale = double(new_height / this->initial_height);

		this->width = new_width;
		this->height = new_height;

		this->notify_updated();
	}
}

void DigMaplet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	IDigDatum* datum = nullptr;
	float ds_rx = x + Width;
	float ds_by = y + Height;

	this->map->rewind();

	while ((datum = this->map->step()) != nullptr) {
		switch (datum->type) {
		case DigDatumType::Line: {
			LineDig* l = static_cast<LineDig*>(datum);
			float2 ep0 = this->position_to_local(l->x, l->y, x, ds_by);
			float2 ep1 = this->position_to_local(l->stop_x, l->stop_y, x, ds_by);
			bool visible = line_visible(ep0.x, ep0.y, ep1.x, ep1.y, x, y, ds_rx, ds_by);

#ifdef _DEBUG
			this->get_logger()->log_message(Log::Debug, L"Line: (%f, %f, %f, %f)%s",
				ep0.x, ep0.y, ep1.x, ep1.y, visible_mark(visible));
#endif

			if (visible) {
				ds->DrawLine(ep0, ep1, vector_colors_ref(l->color), StrokeWidth(l->linewidth), vector_stroke_ref(l->style));
			}
		}; break;
		case DigDatumType::Circle: {
			CircleDig* c = static_cast<CircleDig*>(datum);
			float2 cp = this->position_to_local(c->x, c->y, x, ds_by);
			Size r = this->length_to_local(c->radius);
			bool visible = circle_visible(cp.x, cp.y, r.Width, r.Height, x, y, ds_rx, ds_by);

#ifdef _DEBUG
			{
				Platform::String^ shape = (c->filled ? "disk" : "circle");

				this->get_logger()->log_message(Log::Debug, L"%s: (%f, %f, %f, %f)%s",
					shape->Data(), cp.x, cp.y, r.Width, r.Height, visible_mark(visible));
			}
#endif

			if (visible) {
				if (c->filled) {
					ds->FillEllipse(cp, r.Width, r.Height, vector_colors_ref(c->fill_color));
				}

				ds->DrawEllipse(cp, r.Width, r.Height, vector_colors_ref(c->color), 1.0F, vector_stroke_ref(c->style));
			}
		}; break;
		case DigDatumType::Arc: {
			ArcDig* a = static_cast<ArcDig*>(datum);
			float2 cp = this->position_to_local(a->x, a->y, x, ds_by);
			Size r = this->length_to_local(a->radius);
			double start_deg = a->start_degree - 90.0; // why -90.0?
			double stop_deg = a->stop_degree - 90.0;
			bool visible = circle_visible(cp.x, cp.y, r.Width, r.Height, x, y, ds_rx, ds_by);

#ifdef _DEBUG
			this->get_logger()->log_message(Log::Debug, L"Arc: (%f, %f, %f, %f, %f, %f)%s",
				cp.x, cp.y, r.Width, r.Height, start_deg, stop_deg, visible_mark(visible));
#endif

			if (visible) {
				auto apath = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float sx, sy, ex, ey;

				ellipse_point(r.Width, r.Height, start_deg, &sx, &sy);
				ellipse_point(r.Width, r.Height, stop_deg, &ex, &ey);

				apath->BeginFigure(sx, sy);
				apath->AddArc(float2(ex, ey), r.Width, r.Height, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Large);
				apath->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(apath), cp.x, cp.y,
					vector_colors_ref(a->color), 1.0F, vector_stroke_ref(a->style));
			}
		}; break;
		case DigDatumType::Rectangle: {
			RectangleDig* r = static_cast<RectangleDig*>(datum);
			float2 tl = this->position_to_local(r->x, r->y, x, ds_by);
			Size s = this->length_to_local(r->width, r->height);
			double deg = degrees_normalize(r->rotation);

			if (deg == 0.0) {
				bool visible = rectangle_overlay(tl.x, tl.y, tl.x + s.Width, tl.y + s.Height, x, y, ds_rx, ds_by);

#ifdef _DEBUG
				this->get_logger()->log_message(Log::Debug, L"Rectangle: (%f, %f, %f, %f)%s",
					tl.x, tl.y, s.Width, s.Height, visible_mark(visible));
#endif

				if (visible) {
					ds->DrawRectangle(tl.x, tl.y, s.Width, s.Height, vector_colors_ref(r->color),
						StrokeWidth(r->pen_width), vector_stroke_ref(r->style));
				}
			} else {
				CanvasGeometry^ g = rotate_rectangle(tl.x, tl.y, s.Width, s.Height, deg, tl.x, tl.y);
				Rect box = g->ComputeBounds();
				bool visible = rectangle_overlay(box.X, box.Y, box.X + box.Width, box.Y + box.Height, x, y, ds_rx, ds_by);

#ifdef _DEBUG
				this->get_logger()->log_message(Log::Debug, L"Rotated Rectangle: (%f, %f, %f, %f, %f)%s",
					tl.x, tl.y, s.Width, s.Height, deg, visible_mark(visible));
#endif

				if (visible) {
					ds->DrawGeometry(g, vector_colors_ref(r->color), StrokeWidth(r->pen_width), vector_stroke_ref(r->style));
				}
			}
		}; break;
		default: {
			this->get_logger()->log_message(Log::Info, datum->type.ToString());
		}
		}
	}
}

float2 DigMaplet::position_to_local(double x, double y, float xoff, float yoff) {
	float local_y = -float((x + this->ytranslation) * this->xscale) + yoff;
	float local_x = float((y + this->xtranslation) * this->yscale) + xoff;

	// NOTE the interexchanging of x and y
	// Stupid design, and /or stupid referenced codebase for its lack of explanation

	return float2(local_x, local_y);
}

Size DigMaplet::length_to_local(double width, double height) {
	float local_w = float(((height <= 0.0) ? width : height) * this->yscale);
	float local_h = float(width * this->xscale);

	// NOTE the interexchanging of width and height
	// Stupid design, and /or stupid referenced codebase for its lack of explanation

	return Size(local_w, local_h);
}

float2 DigMaplet::local_to_position(float x, float y, float xoff, float yoff) {
	// TODO
	return float2(x, y);
}
