#include "graphlet/svg/solarpowerletv.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static SolarPowerVStatus default_solarpowerv_status = SolarPowerVStatus::Normal;
static CanvasSolidColorBrush^ default_border_color = Colours::Firebrick;

SolarPowerVStyle WarGrey::SCADA::make_default_solarpowerv_style(SolarPowerVStatus status) {
	SolarPowerVStyle s;

	s.border_color = default_border_color;

	return s;
}

/*************************************************************************************************/
SolarPowerletv::SolarPowerletv(float width, float height)
	: SolarPowerletv(default_solarpowerv_status, width, height) {}

SolarPowerletv::SolarPowerletv(SolarPowerVStatus default_status, float width, float height)
	: Svglet(default_status, &make_default_solarpowerv_style, width, height) {}

Platform::String^ SolarPowerletv::name() {
	return "SolarPowerPanel";
}

void SolarPowerletv::apply_style(SolarPowerVStyle& style) {
	this->set_stroke_color("border", style.border_color);
}
