#include "graphlet/filesystem/project/xyzlet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::UI;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
Xyzlet::Xyzlet(XyzDoc^ depths, float diff_ft_times) : doc_xyz(nullptr), diff_multiple(diff_ft_times), default_color(Colours::GhostWhite) {
	this->enable_resizing(false);
	this->enable_events(false, false);
	this->camouflage(true);

	this->merge(depths);
}

void Xyzlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(y));
}

void Xyzlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if ((this->master_map != nullptr) && (this->doc_xyz != nullptr)) {
		float ds_x = x - this->num_size;
		float ds_y = y - this->num_size;
		float ds_rx = x + Width + this->num_size;
		float ds_by = y + Height + this->num_size;
		float2 last_pos(-infinity_f, -infinity_f);
		float delta = this->font->FontSize * this->diff_multiple;
		CanvasCachedGeometry^ gs[2];
		float whole_width;

		for (auto it = this->doc_xyz->depths.begin(); it != this->doc_xyz->depths.end(); it++) {
			if ((this->plot == nullptr) || (this->plot->in_range(it->z))) {
				float2 pos = this->master_map->position_to_local(it->x, it->y, x, y);

				if (flin(ds_x, pos.x, ds_rx) && flin(ds_y, pos.y, ds_by)) {
					float distance = points_distance(last_pos, pos);

					if (distance >= delta) {
						CanvasSolidColorBrush^ color = this->default_color;

						if (this->plot != nullptr) {
							color = this->plot->depth_color(it->z, color);
						}

						if (color->Color.A > 0) {
							this->find_or_create_depth_geometry(it->z, gs, &whole_width);

							ds->DrawCachedGeometry(this->location, pos.x - this->loc_xoff, pos.y - this->loc_yoff, color);
							ds->DrawCachedGeometry(gs[0], pos.x - whole_width - this->loc_xoff, pos.y - this->num_size * 0.8F, color);
							ds->DrawCachedGeometry(gs[1], pos.x + this->loc_xoff, pos.y - this->num_size * 0.382F, color);
						}

						last_pos = pos;
					}
				}
			}
		}
	}
}

void Xyzlet::on_map_updated() {
	if (this->master_map != nullptr) {
		float ftsize = this->master_map->plain_font_size();
		TextExtent te;

		if ((this->font == nullptr) || (this->font->FontSize != ftsize)) {
			this->font = make_text_format("Arial", ftsize);
			this->location = geometry_freeze(paragraph(".", this->font, &te));
			this->loc_xoff = te.lspace * 0.5F + (te.width - te.rspace) * 0.5F;
			this->loc_yoff = te.tspace * 0.5F + (te.height - te.bspace) * 0.5F;
			this->num_size = te.height;

			this->wholes.clear();
			this->fractions.clear();

			this->notify_updated();
		}
	}
}

void Xyzlet::set_color_schema(ColorPlotlet* plot, CanvasSolidColorBrush^ fallback) {
	if (this->plot != plot) {
		this->plot = plot;
		this->notify_updated();
	}

	if ((this->default_color != fallback) && (fallback != nullptr)) {
		this->default_color = fallback;
		this->notify_updated();
	}
}

void Xyzlet::merge(XyzDoc^ depths) {
	if (depths != nullptr) {
		if (this->doc_xyz == nullptr) {
			this->doc_xyz = depths;
			this->notify_updated();
		} else if (this->doc_xyz != depths) {
			this->doc_xyz->append(depths);
			this->notify_updated();
		}
	}
}

void Xyzlet::find_or_create_depth_geometry(double depth, CanvasCachedGeometry^ gs[], float* whole_width) {
	int whole = int(flfloor(depth));
	int fraction = int(flfloor(depth * 100.0)) - (whole * 100);
	auto wpair = this->wholes.find(whole);
	auto fpair = this->fractions.find(fraction);

	SET_BOX(whole_width, this->num_size * ((whole < 10) ? 0.5F : 1.0F));
	if (wpair != this->wholes.end()) {
		gs[0] = wpair->second;
	} else {
		gs[0] = geometry_freeze(paragraph(whole.ToString(), this->font));
		this->wholes.insert(std::pair<int, CanvasCachedGeometry^>(whole, gs[0]));
	}

	if (fpair != this->fractions.end()) {
		gs[1] = fpair->second;
	} else {
		gs[1] = geometry_freeze(paragraph(fxstring(fraction, 2), this->font));
		this->fractions.insert(std::pair<int, CanvasCachedGeometry^>(fraction, gs[1]));
	}
}
