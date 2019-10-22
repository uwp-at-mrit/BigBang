#include "graphlet/filesystem/project/digmaplet.hpp"

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

/*************************************************************************************************/
DigMaplet::DigMaplet(DigDoc^ map, double width, double height, double fontsize_times, float plain_fontsize)
	: map(map), width(float(width)), height(float(height)), fstimes(fontsize_times), stimes(1.0), tstep(32.0F) {
	this->enable_resizing(false);
	this->enable_events(false, false);
	
	/** NOTE
	 * Do not move these lines to DigMaplet::construct(),
	 * Diglet::on_appdata() requires the scale to locate icons.
	 */
	this->map->fill_enclosing_box(&this->geo_x, &this->geo_y, &this->geo_width, &this->geo_height);
	this->_scale = flmin(this->width / this->geo_height, this->height / this->geo_width);
	this->plainfont = make_text_format(plain_fontsize);
}

void DigMaplet::construct() {
	this->center();
}

void DigMaplet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void DigMaplet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	IDigDatum* dig = nullptr;
	float ds_rx = x + Width;
	float ds_by = y + Height;

	this->map->rewind();

	while ((dig = this->map->step()) != nullptr) {
		bool visible = false;

		if ((dig->lx < dig->rx) || (dig->ty < dig->by)) { // Lines may be zero-width or zero-height
			float2 loc_lt = this->position_to_local(dig->lx, dig->ty, x, y);
			float2 loc_rb = this->position_to_local(dig->rx, dig->by, x, y);

			// NOTE the y-axis has been flipped
			visible = rectangle_overlay(loc_lt.x, loc_rb.y, loc_rb.x, loc_lt.y, x, y, ds_rx, ds_by);
		} else {
			if (((dig->lx == dig->rx) && (dig->ty == dig->by)) || (dig->type == DigDatumType::FontText)) {
				this->preshape(dig);
			}
			
			if ((dig->lx > dig->rx) && (dig->ty > dig->by)) { // also see this::preshape
				float2 pos = this->position_to_local(dig->x, dig->y, x, y);
				float loc_lx = pos.x + float(dig->lx);
				float loc_ty = pos.y + float(dig->ty);
				float loc_rx = loc_lx + float(dig->lx - dig->rx);
				float loc_by = loc_ty + float(dig->ty - dig->by);

				visible = rectangle_overlay(loc_lx, loc_ty, loc_rx, loc_by, x, y, ds_rx, ds_by);
			}
		}

		if (visible) {
			switch (dig->type) {
			case DigDatumType::Line: {
				LineDig* l = static_cast<LineDig*>(dig);
				float2 ep0 = this->position_to_local(l->x, l->y, x, y);
				float2 ep1 = this->position_to_local(l->stop_x, l->stop_y, x, y);
				
				ds->DrawLine(ep0, ep1, vector_colors_ref(l->color), StrokeWidth(l->linewidth), vector_stroke_ref(l->style));
			}; break;
			case DigDatumType::Circle: {
				CircleDig* c = static_cast<CircleDig*>(dig);
				float2 cp = this->position_to_local(c->x, c->y, x, y);
				Size r = this->length_to_local(c->radius);
				
				if (c->filled) {
					ds->FillEllipse(cp, r.Width, r.Height, vector_colors_ref(c->fill_color));
				}

				ds->DrawEllipse(cp, r.Width, r.Height, vector_colors_ref(c->color), 1.0F, vector_stroke_ref(c->style));
			}; break;
			case DigDatumType::ShoreLine: {
				ShoreLineDig* l = static_cast<ShoreLineDig*>(dig);
				CanvasPathBuilder^ sl = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(dig->x, dig->y, x, y);

				sl->BeginFigure(start);
				for (size_t idx = 0; idx < l->poly_xs.size(); idx++) {
					float2 dot = this->position_to_local(l->poly_xs[idx], l->poly_ys[idx], x, y);

					sl->AddLine(dot);
				}
				sl->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(sl), vector_colors_ref(0LL), 1.0F);
			}; break;
			case DigDatumType::PolyLine: {
				PolyLineDig* l = static_cast<PolyLineDig*>(dig);
				CanvasPathBuilder^ pl = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(dig->x, dig->y, x, y);

				pl->BeginFigure(start);
				for (size_t idx = 0; idx < l->poly_xs.size(); idx++) {
					float2 dot = this->position_to_local(l->poly_xs[idx], l->poly_ys[idx], x, y);

					pl->AddLine(dot);
				}
				pl->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(pl), vector_colors_ref(l->color),
					StrokeWidth(l->width), vector_stroke_ref(l->style));
			}; break;
			case DigDatumType::PolyBezier: {
				PolyBezierDig* b = static_cast<PolyBezierDig*>(dig);
				CanvasPathBuilder^ pb = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
				float2 start = this->position_to_local(dig->x, dig->y, x, y);
				size_t idxmax = b->poly_xs.size() - 2;
				size_t idx = 0;

				pb->BeginFigure(start);
				while (1) {
					if (idx < idxmax) {
						float2 cp1 = this->position_to_local(b->poly_xs[idx + 0], b->poly_ys[idx + 0], x, y);
						float2 cp2 = this->position_to_local(b->poly_xs[idx + 1], b->poly_ys[idx + 1], x, y);
						float2 cp3 = this->position_to_local(b->poly_xs[idx + 2], b->poly_ys[idx + 2], x, y);
		
						pb->AddCubicBezier(cp1, cp2, cp3);
						idx += 3;
					} else if (idx == idxmax) {
						float2 cp1 = this->position_to_local(b->poly_xs[idx + 0], b->poly_ys[idx + 0], x, y);
						float2 cp2 = this->position_to_local(b->poly_xs[idx + 1], b->poly_ys[idx + 1], x, y);

						pb->AddQuadraticBezier(cp1, cp2);
						idx += 2;
					} else {
						break;
					}
				}
				pb->EndFigure(CanvasFigureLoop::Open);

				ds->DrawGeometry(CanvasGeometry::CreatePath(pb), vector_colors_ref(b->color), StrokeWidth(b->width), vector_stroke_ref(b->style));
			}; break;
			case DigDatumType::Arc: {
				ArcDig* a = static_cast<ArcDig*>(dig);
				float2 cp = this->position_to_local(a->x, a->y, x, y);
				Size r = this->length_to_local(a->radius);
				CanvasGeometry^ g = nullptr;
				
				{ // draw arc
					double start_deg = a->start_degree - 90.0;
					double stop_deg = a->stop_degree - 90.0;

					// NOTE that modifyDIG uses the lefthand coordinate system
					//   the degrees therefore should sweep 90.0 degrees counterclockwise
					// Stupid design, and/or stupid referenced codebase for lacking of explanation

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
				RectangleDig* r = static_cast<RectangleDig*>(dig);
				float2 tl = this->position_to_local(r->x, r->y, x, y);
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
			case DigDatumType::Text: { // also see DigDatumType::Rectangle, but modifyDIG handles normal texts accurately.
				float2 tp = this->position_to_local(dig->x, dig->y, x, y);
				float text_height = float(dig->ty - dig->by); // see this::preshape
				
				ds->FillGeometry(this->plaintexts[dig->name], tp.x, tp.y - text_height, vector_colors_ref(0LL));
			}; break;
			case DigDatumType::Number: { // also see DigDatumType::Rectangle, but modifyDIG handles depth accurately.
				float2 tp = this->position_to_local(dig->x, dig->y, x, y);
				float depth_height = float(dig->ty - dig->by); // see this::preshape

				ds->FillGeometry(this->numtexts[dig->name], tp.x, tp.y - depth_height, vector_colors_ref(0LL));
			}; break;
			case DigDatumType::FontText: { // also see DigDatumType::Rectangle
				FontTextDig* td = static_cast<FontTextDig*>(dig);
				
				if (td->font_size > 0LL) {
					float2 tp = this->position_to_local(dig->x, dig->y, x, y);
					
					ds->FillGeometry(this->fonttexts[td], tp, vector_colors_ref(td->color));
				}
			}; break;
			}
		} else {
#ifdef _DEBUG
		this->get_logger()->log_message(Log::Debug,
			L"invisible: %s; view window: (%f, %f), (%f, %f); scale: %f; translation: [%f, %f]",
			dig->to_string()->Data(), x, y, ds_rx, ds_by, this->scale(), this->xtranslation, this->ytranslation);
#endif
		}
	}
}

float2 DigMaplet::local_to_position(float x, float y, float xoff, float yoff) {
	double ss = this->actual_scale();
	float ty = yoff + this->height + this->ytranslation;
	float tx = xoff + this->xtranslation;
	float gy = float((x - tx) / ss + this->geo_y);
	float gx = float((ty - y) / ss + this->geo_x);

	// NOTE that modifyDIG uses lefthand coordinate system
	//   the x and y therefore should be interchanged before drawing
	// Stupid design, and/or stupid referenced codebase for lacking of explanation

	return float2(gx, gy);
}

float2 DigMaplet::position_to_local(double x, double y, float xoff, float yoff) {
	double ss = this->actual_scale();
	float ty = yoff + this->height + this->ytranslation;
	float tx = xoff + this->xtranslation;
	float local_y = -float((x - this->geo_x) * ss) + ty;
	float local_x = float((y - this->geo_y) * ss) + tx;

	// NOTE that modifyDIG uses lefthand coordinate system
	//   the x and y therefore should be interchanged before drawing
	// Stupid design, and/or stupid referenced codebase for lacking of explanation

	return float2(local_x, local_y);
}

Size DigMaplet::length_to_local(double width, double height) {
	double ss = this->actual_scale();
	float local_w = float(((height <= 0.0) ? width : height) * ss);
	float local_h = float(width * ss);

	// NOTE that modifyDIG uses lefthand coordinate system
	//   the width and height therefore should be interchanged before drawing
	// Stupid design, and/or stupid referenced codebase for its lack of explanation

	return Size(local_w, local_h);
}

void DigMaplet::transform(MapMove move) {
	double posttimes = this->stimes;
	float sx = this->width * 0.5F;
	float sy = this->height * 0.5F;
	
	switch (move) {
	case MapMove::Left: this->xtranslation -= this->tstep; break;
	case MapMove::Right: this->xtranslation += this->tstep; break;
	case MapMove::Up: this->ytranslation -= this->tstep; break;
	case MapMove::Down: this->ytranslation += this->tstep; break;
	case MapMove::ScaleUp: {
		if (this->stimes >= 1.0) {
			posttimes = this->stimes + 1.0;
		} else {
			posttimes = this->stimes * 2.0;
		}

		this->scale_transform(posttimes, sx, sy);
	}; break;
	case MapMove::ScaleDown: {
		if (this->stimes > 1.0) {
			posttimes = this->stimes - 1.0;
		} else {
			posttimes = this->stimes * 0.5;
		}

		this->scale_transform(posttimes, sx, sy);
	}; break;
	case MapMove::Reset: {
		posttimes = 1.0;
		this->scale_transform(posttimes, sx, sy);
		this->center();
	}; break;
	}

	this->notify_updated();
}

void DigMaplet::scale_transform(double stimes, float anchor_x, float anchor_y) {
	if (this->stimes != stimes) {
		float2 anchor = this->local_to_position(anchor_x, anchor_y, 0.0F, 0.0F);

		this->stimes = stimes;
		anchor = this->position_to_local(anchor.x, anchor.y, 0.0F, 0.0F);

		this->xtranslation -= (anchor.x - anchor_x);
		this->ytranslation -= (anchor.y - anchor_y);

		this->fonttexts.clear();
	}
}

void DigMaplet::center() {
	this->center_at(flnan, flnan);
}

void DigMaplet::center_at(double x, double y) {
	double anchor_x = x;
	double anchor_y = y;

	if (flisnan(anchor_x)) {
		anchor_x = this->geo_x + this->geo_width * 0.5F;
	}

	if (flisnan(anchor_y)) {
		anchor_y = this->geo_y + this->geo_height * 0.5F;
	}

	this->xtranslation = 0.0F;
	this->ytranslation = 0.0F;

	{ // do translating
		float2 cdot = this->position_to_local(anchor_x, anchor_y);

		this->xtranslation = this->width * 0.5F - cdot.x;
		this->ytranslation = this->height * 0.5F - cdot.y;
	}
}

float DigMaplet::scaled_font_size(long long fontsize) {
	return float(double(fontsize) * this->fstimes * this->actual_scale());
}

float DigMaplet::plain_font_size() {
	return this->plainfont->FontSize;
}

double DigMaplet::actual_scale() {
	return this->_scale * this->stimes;
}

void DigMaplet::fill_anchor_position(double fx, double fy, double* x, double* y) {
	SET_BOX(x, this->geo_x + this->geo_width * fx);
	SET_BOX(y, this->geo_y + this->geo_height * fy);
}

void DigMaplet::preshape(IDigDatum* dig) {
	Rect tbx(0.0F, 0.0F, 0.0F, 0.0F);

	switch (dig->type) {
	case DigDatumType::Text: {
		if (this->plaintexts.find(dig->name) == this->plaintexts.end()) {
			CanvasTextLayout^ tlt = make_text_layout(dig->name, this->plainfont);
			CanvasGeometry^ g = paragraph(tlt);
			
			tbx = tlt->LayoutBounds;
			this->plaintexts.insert(std::pair<Platform::String^, CanvasGeometry^>(dig->name, g));
		} else {
			tbx = this->plaintexts[dig->name]->ComputeBounds();
		}
	}; break;
	case DigDatumType::Number: {
		if (this->numtexts.find(dig->name) == this->numtexts.end()) {
			CanvasTextLayout^ tlt = make_text_layout(dig->name, this->plainfont);
			CanvasGeometry^ g = paragraph(tlt);
			CanvasPathBuilder^ bp = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
			
			tbx = tlt->LayoutBounds;
			
			{ // decorate depth
				float bhy = tbx.Height * 0.75F;

				bp->BeginFigure(0.0F, bhy);
				bp->AddLine(0.0F, tbx.Height);
				bp->AddLine(tbx.Width, tbx.Height);
				bp->AddLine(tbx.Width, bhy);
				bp->EndFigure(CanvasFigureLoop::Open);

				this->numtexts.insert(std::pair<Platform::String^, CanvasGeometry^>(dig->name,
					geometry_union(g, geometry_stroke(CanvasGeometry::CreatePath(bp), 1.0F))));
			}
		} else {
			tbx = this->numtexts[dig->name]->ComputeBounds();
		}
	}; break;
	case DigDatumType::FontText: { // also see DigDatumType::Rectangle
		FontTextDig* td = static_cast<FontTextDig*>(dig);

		if (td->font_size > 0LL) {
			if (this->fonttexts.find(td) == this->fonttexts.end()) {
				float font_size = this->scaled_font_size(td->font_size);
				CanvasGeometry^ tlt = paragraph(dig->name, make_text_format(td->font_name, font_size));

				if (degrees_normalize(td->rotation) != 0.0) {
					tlt = geometry_rotate(tlt, td->rotation, 0.0F, 0.0F);
				}

				tbx = tlt->ComputeBounds();
				this->fonttexts.insert(std::pair<FontTextDig*, CanvasGeometry^>(td, tlt));
			} else {
				tbx = this->fonttexts[td]->ComputeBounds();
			}
		}
	}; break;
	}

	dig->lx = tbx.X;
	dig->ty = tbx.Y;

	/** NOTE
	 * Dig items handled here are not dot-based, and their sizes might be computed dynamcally,
	 * thus, the `rx` and `by` are deliberately set to less than their counterparts.
	 *
	 * see this::draw
	 */
	dig->rx = dig->lx - tbx.Width;
	dig->by = dig->ty - tbx.Height;
}
