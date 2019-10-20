#include "graphlet/filesystem/configuration/colorplotlet.hpp"

#include "datum/file.hpp"

#include "colorspace.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class CP { ColorPlot, Range, RGBA };
}

/*************************************************************************************************/
ColorPlotlet::ColorPlotlet(Platform::String^ plot, float width, float height, Platform::String^ ext, Platform::String^ rootdir) : width(width), height(height) {
	if (plot != nullptr) {
		this->ms_appdata_config = ms_appdata_file(plot, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("colorplot", ext));
	}

	if (this->height == 0.0F) {
		this->height = this->width;
	} else if (this->height < 0.0F) {
		this->height *= -this->width;
	}
}

ColorPlotlet::~ColorPlotlet() {
	this->unload(this->ms_appdata_config);
}

void ColorPlotlet::construct() {
	this->load(this->ms_appdata_config);
	this->font = make_bold_text_format("Arial", 12.0F);
}

void ColorPlotlet::on_appdata(Uri^ gps, ColorPlot^ plot_config) {
	this->plot_config = plot_config;
}

bool ColorPlotlet::ready() {
	return (this->plot_config != nullptr);
}

void ColorPlotlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void ColorPlotlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->plot_config != nullptr) {
		float plot_height = this->height / float(ColorPlotSize);

		for (unsigned int idx = 0; idx < ColorPlotSize; idx++) {
			CanvasSolidColorBrush^ c = this->plot_config->colors[idx];

			if (c != nullptr) {
				float iy = y + plot_height * float(idx) - 1.0F;

				ds->FillRectangle(x - 1.0F, iy, this->width + 2.0F, plot_height + 2.0F, this->plot_config->colors[idx]);
				ds->DrawText(flstring(this->plot_config->depths[idx], 2), x + 1.0F, iy - 1.0F, Colours::GhostWhite, this->font);
			}
		}
	}

	ds->DrawRectangle(x + 0.5F, y + 0.5F, this->width - 1.0F, this->height - 1.0F, Colours::GrayText);
}

/*************************************************************************************************/
bool ColorPlotlet::in_range(double depth) {
	return ((this->plot_config != nullptr)
		&& flin(this->plot_config->min_depth, depth, this->plot_config->max_depth));
}

CanvasSolidColorBrush^ ColorPlotlet::depth_color(double depth, CanvasSolidColorBrush^ fallback) {
	CanvasSolidColorBrush^ c = fallback;

	if (this->plot_config != nullptr) {
		for (int idx = ColorPlotSize - 1; idx >= 0; idx--) {
			if (this->plot_config->depths[idx] <= depth) {
				if (this->plot_config->enableds[idx] && (this->plot_config->colors[idx] != nullptr)) {
					c = this->plot_config->colors[idx];
					break;
				}
			}
		}
	}

	return c;
}

/*************************************************************************************************/
ColorPlot^ ColorPlotlet::clone_plot(ColorPlot^ dest) {
	ColorPlot^ clone = ((dest == nullptr) ? ref new ColorPlot() : dest);

	clone->refresh(this->plot_config);

	return clone;
}

void ColorPlotlet::refresh(ColorPlot^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
ColorPlot^ ColorPlot::load(Platform::String^ path) {
	ColorPlot^ cs = nullptr;
	Platform::String^ wtype;
	std::filebuf src;
	
	if (open_input_binary(src, path)) {
		cs = ref new ColorPlot();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (CP::ColorPlot.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (CP::Range.ToString()->Equals(wtype)) {
					cs->min_depth = read_flonum(src);
					cs->max_depth = read_flonum(src);
				} else if (CP::RGBA.ToString()->Equals(wtype)) {
					unsigned long long idx = read_natural(src);

					if (idx < ColorPlotSize) {
						cs->depths[idx] = read_flonum(src);

						{ // do not merge these lines since the right-most argument is evaluated first
							unsigned int rgb = (unsigned int)(read_natural(src));
							double alpha = read_flonum(src);

							cs->colors[idx] = Colours::make(rgb, alpha);
						}

						cs->enableds[idx] = read_bool(src);
					}
				}

				discard_this_line(src);
			}
		}
	}

	return cs;
}

bool ColorPlot::save(ColorPlot^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;
	size_t ptsize = sizeof(double2);

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, CP::ColorPlot, true);

		write_wtext(v_config, CP::Range);
		v_config << " " << self->min_depth << " " << self->max_depth;
		write_newline(v_config);

		for (unsigned int idx = 0; idx < ColorPlotSize; idx++) {
			CanvasSolidColorBrush^ c = self->colors[idx];

			write_wtext(v_config, CP::RGBA);
			v_config << " " << idx << " " << self->depths[idx];

			if (c != nullptr) {
				v_config << " " << color_to_hexadecimal(c->Color) << " " << color_char_to_double(c->Color.A) << " ";
				write_bool(v_config, self->enableds[idx]);
			} else {
				v_config << " " << 0 << " " << 1.0 << " ";
				write_bool(v_config, false);
			}

			write_newline(v_config);
		}

		v_config.flush();

		okay = true;
	}

	return okay;
}

ColorPlot::ColorPlot(ColorPlot^ src) {
	this->refresh(src);
}

void ColorPlot::refresh(ColorPlot^ src) {
	if ((src != nullptr) && (this != src)) {
		for (unsigned idx = 0; idx < ColorPlotSize; idx++) {
			this->depths[idx] = src->depths[idx];
			this->colors[idx] = src->colors[idx];
			this->enableds[idx] = src->enableds[idx];
		}

		this->min_depth = src->min_depth;
		this->max_depth = src->max_depth;
	}
}
