#include <map>

#include "graphlet/symbol/dig/digmaplet.hpp"

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
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
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

namespace {
	private ref class VectorText sealed {
	internal:
		VectorText(CanvasGeometry^ g, Rect& box) : g(g) {
			this->x = box.X;
			this->y = box.Y;
			this->width = box.Width;
			this->height = box.Height;
		}

	public:
		CanvasGeometry^ shape(float* x, float* y, float* width, float* height) {
			SET_VALUES(x, this->x, y, this->y);
			SET_VALUES(width, this->width, height, this->height);

			return g;
		}

	private:
		CanvasGeometry^ g;
		float x;
		float y;
		float width;
		float height;
	};
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
		float2 llt = this->position_to_local(datum->lx, datum->ty, x, ds_by);
		float2 lrb = this->position_to_local(datum->rx, datum->by, x, ds_by);

		// NOTE the y-axis has been flipped
		bool visible = ((datum->lx == datum->rx) && (datum->ty == datum->by)) // for texts
			|| rectangle_overlay(llt.x, lrb.y, lrb.x, llt.y, x, y, ds_rx, ds_by);

		if (visible) {
			switch (datum->type) {
			case DigDatumType::Line: {
				LineDig* l = static_cast<LineDig*>(datum);
				float2 ep0 = this->position_to_local(l->x, l->y, x, ds_by);
				float2 ep1 = this->position_to_local(l->stop_x, l->stop_y, x, ds_by);
				
				ds->DrawLine(ep0, ep1, vector_colors_ref(l->color), StrokeWidth(l->linewidth), vector_stroke_ref(l->style));
			}; break;
			case DigDatumType::Circle: {
				CircleDig* c = static_cast<CircleDig*>(datum);
				float2 cp = this->position_to_local(c->x, c->y, x, ds_by);
				Size r = this->length_to_local(c->radius);
				
				if (c->filled) {
					ds->FillEllipse(cp, r.Width, r.Height, vector_colors_ref(c->fill_color));
				}

				ds->DrawEllipse(cp, r.Width, r.Height, vector_colors_ref(c->color), 1.0F, vector_stroke_ref(c->style));
			}; break;
			case DigDatumType::Arc: {
				ArcDig* a = static_cast<ArcDig*>(datum);
				float2 cp = this->position_to_local(a->x, a->y, x, ds_by);
				Size r = this->length_to_local(a->radius);
				CanvasGeometry^ g = nullptr;
				
				{ // draw arc
					double start_deg = a->start_degree - 90.0;
					double stop_deg = a->stop_degree - 90.0;

					// NOTE that modifyDIG uses the lefthand coordinate system
					//   the degrees therefore should sweep 90.0 degrees counterclockwise
					// Stupid design, and/or stupid referenced codebase for its lack of explanation

				    // NOTE: the arc is ensured to be drawn counterclockwise
					if (start_deg > stop_deg) {
						g = arc(start_deg, stop_deg, r.Width, r.Height);
					} else {
						g = arc(start_deg + 360.0, stop_deg, r.Width, r.Height);
					}

					ds->DrawGeometry(g, cp.x, cp.y, vector_colors_ref(a->color), 1.0F, vector_stroke_ref(a->style));
				}
			}; break;
			case DigDatumType::Rectangle: {
				RectangleDig* r = static_cast<RectangleDig*>(datum);
				float2 tl = this->position_to_local(r->x, r->y, x, ds_by);
				Size s = this->length_to_local(r->width, r->height);
				double deg = degrees_normalize(r->rotation);

				/** WARNING
				 * The modifyDIG does not handle rectangles accurately, DigMaplet follows it for the sake of compatibility.
				 * Nevertheless, rectangles should be translated or flipped vertically since modifyDIG uses the lefthand coordinate system.
				 * Rotation makes it even harder since its center point is the left-top one which is actually indicating the left-bottom one.
				 *
				 * By the way, this bug is not a big deal since rectangles are less used in real world projects.
				 * Designers and users have no chance to do investigating on the spot directly as well,
				 *   modifyDIG will transform and restore raw geodesy data for them,
				 *   the bug is concealed unconsciously.
				 *
				 * Also see Diglet::on_appdata, how the location in screen of rectangles should be adjusted.
				 */

				if (deg == 0.0) {
					ds->DrawRectangle(tl.x, tl.y, s.Width, s.Height, vector_colors_ref(r->color),
						StrokeWidth(r->pen_width), vector_stroke_ref(r->style));
				} else {
					CanvasGeometry^ g = rotate_rectangle(tl.x, tl.y, s.Width, s.Height, deg, tl.x, tl.y);
					Rect box = g->ComputeBounds();

					ds->DrawGeometry(g, vector_colors_ref(r->color), StrokeWidth(r->pen_width), vector_stroke_ref(r->style));
				}
			}; break;
			case DigDatumType::ShoreLine: {
				ShoreLineDig* l = static_cast<ShoreLineDig*>(datum);
				CanvasPathBuilder^ sl = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(datum->x, datum->y, x, ds_by);

				sl->BeginFigure(start);
				for (size_t idx = 0; idx < l->poly_xs.size(); idx++) {
					float2 dot = this->position_to_local(l->poly_xs[idx], l->poly_ys[idx], x, ds_by);

					sl->AddLine(dot);
				}
				sl->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(sl), vector_colors_ref(0LL), 1.0F);
			}; break;
			case DigDatumType::PolyLine: {
				PolyLineDig* l = static_cast<PolyLineDig*>(datum);
				CanvasPathBuilder^ pl = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(datum->x, datum->y, x, ds_by);

				pl->BeginFigure(start);
				for (size_t idx = 0; idx < l->poly_xs.size(); idx++) {
					float2 dot = this->position_to_local(l->poly_xs[idx], l->poly_ys[idx], x, ds_by);

					pl->AddLine(dot);
				}
				pl->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(pl), vector_colors_ref(l->color),
					StrokeWidth(l->width), vector_stroke_ref(l->style));
			}; break;
			case DigDatumType::PolyBezier: {
				PolyBezierDig* b = static_cast<PolyBezierDig*>(datum);
				CanvasPathBuilder^ pb = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(datum->x, datum->y, x, ds_by);
				size_t idxmax = b->poly_xs.size() - 2;
				size_t idx = 0;

				pb->BeginFigure(start);
				while (1) {
					if (idx < idxmax) {
						float2 cp1 = this->position_to_local(b->poly_xs[idx + 0], b->poly_ys[idx + 0], x, ds_by);
						float2 cp2 = this->position_to_local(b->poly_xs[idx + 1], b->poly_ys[idx + 1], x, ds_by);
						float2 cp3 = this->position_to_local(b->poly_xs[idx + 2], b->poly_ys[idx + 2], x, ds_by);
		
						pb->AddCubicBezier(cp1, cp2, cp3);
						idx += 3;
					} else if (idx == idxmax) {
						float2 cp1 = this->position_to_local(b->poly_xs[idx + 0], b->poly_ys[idx + 0], x, ds_by);
						float2 cp2 = this->position_to_local(b->poly_xs[idx + 1], b->poly_ys[idx + 1], x, ds_by);

						pb->AddQuadraticBezier(cp1, cp2);
						idx += 2;
					} else {
						break;
					}
				}
				pb->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(pb), vector_colors_ref(b->color), StrokeWidth(b->width), vector_stroke_ref(b->style));
			}; break;
			case DigDatumType::Text: case DigDatumType::Depth: { // also see DigDatumType::Rectangle
				float2 tp = this->position_to_local(datum->x, datum->y, x, ds_by);
				CanvasTextLayout^ tl = nullptr;

				if (this->plaintexts.find(datum->name) == this->plaintexts.end()) {
					CanvasTextLayout^ tlt = make_text_layout(datum->name, make_text_format());
					CanvasGeometry^ g = paragraph(tlt);
					Rect tbx = tlt->LayoutBounds;

					if (datum->type == DigDatumType::Depth) {
						CanvasPathBuilder^ bp = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
						float bhy = tbx.Height * 0.5F;

						bp->BeginFigure(0.0F, bhy);
						bp->AddLine(0.0F, tbx.Height);
						bp->AddLine(tbx.Width, tbx.Height);
						bp->AddLine(tbx.Width, bhy);
						bp->EndFigure(CanvasFigureLoop::Open);

						g = geometry_union(g, geometry_stroke(CanvasGeometry::CreatePath(bp), 1.0F));
					}
					
					this->plaintexts.insert(std::pair<Platform::String^, Platform::Object^>(datum->name, ref new VectorText(g, tbx)));
				}

				this->draw_text(ds, this->plaintexts[datum->name], tp, 0LL, datum, x, y, ds_rx, ds_by);
			}; break;
			case DigDatumType::FontText: { // also see DigDatumType::Rectangle
				FontTextDig* td = static_cast<FontTextDig*>(datum);

				if (td->font_size > 0LL) {
					float2 tp = this->position_to_local(datum->x, datum->y, x, ds_by);
					CanvasTextLayout^ tl = nullptr;

					if (this->plaintexts.find(datum->name) == this->plaintexts.end()) {
						CanvasGeometry^ tlt = paragraph(datum->name, make_text_format(td->font_name, float(td->font_size)));

						if (degrees_normalize(td->rotation) != 0.0) {
							tlt = geometry_rotate(tlt, td->rotation, 0.0F, 0.0F);
						}

						this->fonttexts.insert(std::pair<FontTextDig*, Platform::Object^>(td,
							ref new VectorText(tlt, tlt->ComputeBounds())));
					}

					this->draw_text(ds, this->fonttexts[td], tp, td->color, datum, x, y, ds_rx, ds_by);
				}
			}; break;
			}
		} else {
#ifdef _DEBUG
			this->get_logger()->log_message(Log::Debug, L"invisible: %s", datum->to_string()->Data());
#endif
		}
	}
}

float2 DigMaplet::position_to_local(double x, double y, float xoff, float yoff) {
	float local_y = -float((x + this->ytranslation) * this->xscale) + yoff;
	float local_x = float((y + this->xtranslation) * this->yscale) + xoff;

	// NOTE that modifyDIG uses lefthand coordinate system
	//   the x and y therefore should be interchanged before drawing
	// Stupid design, and/or stupid referenced codebase for its lack of explanation

	return float2(local_x, local_y);
}

Size DigMaplet::length_to_local(double width, double height) {
	float local_w = float(((height <= 0.0) ? width : height) * this->yscale);
	float local_h = float(width * this->xscale);

	// NOTE that modifyDIG uses lefthand coordinate system
	//   the width and height therefore should be interchanged before drawing
	// Stupid design, and/or stupid referenced codebase for its lack of explanation

	return Size(local_w, local_h);
}

float2 DigMaplet::local_to_position(float x, float y, float xoff, float yoff) {
	// TODO
	return float2(x, y);
}

void DigMaplet::draw_text(CanvasDrawingSession^ ds, Platform::Object^ vtobj, float2& tp, long long color
	, IDigDatum* dig, float lx, float ty, float rx, float by) {
	float vtx, vty, vtwidth, vtheight;
	CanvasGeometry^ shape = static_cast<VectorText^>(vtobj)->shape(&vtx, &vty, &vtwidth, &vtheight);

	vtx += tp.x;
	vty += tp.y;
	
	if (rectangle_overlay(vtx, vty, vtx + vtwidth, vty + vtheight, lx, ty, rx, by)) {
		ds->FillGeometry(shape, tp, vector_colors_ref(color));
	} else {
#ifdef _DEBUG
		this->get_logger()->log_message(Log::Debug, L"invisible: %s", dig->to_string()->Data());
#endif
	}
}
