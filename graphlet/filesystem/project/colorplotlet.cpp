#include "graphlet/filesystem/project/colorplotlet.hpp"

#include "datum/path.hpp"
#include "datum/file.hpp"

#include "tongue.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "polar.hpp"
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
	private enum class CP { ColorPlot };
}

/*************************************************************************************************/
ColorPlotlet::ColorPlotlet(Platform::String^ plot, float size, Platform::String^ ext, Platform::String^ rootdir) : size(size) {
	if (plot != nullptr) {
		this->ms_appdata_config = ms_appdata_file(plot, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("colorplot", ext));
	}
}

ColorPlotlet::~ColorPlotlet() {
	this->unload(this->ms_appdata_config);
}

void ColorPlotlet::construct() {
	this->load(this->ms_appdata_config);
}

void ColorPlotlet::on_appdata(Uri^ gps, ColorPlot^ gps_config) {
	this->plot_config = gps_config;
}

bool ColorPlotlet::ready() {
	return (this->plot_config != nullptr);
}

void ColorPlotlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}

void ColorPlotlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRectangle(x, y, Width, Height, Colours::GhostWhite);
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
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		cs = ref new ColorPlot();
		wtype = read_wtext(src);
		discard_this_line(src);
	}

	return cs;
}

bool ColorPlot::save(ColorPlot^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;
	size_t ptsize = sizeof(double2);

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, CP::ColorPlot) << "\n\r" << std::endl;

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
	}
}
