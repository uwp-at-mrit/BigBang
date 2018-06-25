#include "graphlet/svg/solarpowerletv.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
SolarPowerletv::SolarPowerletv(float width, float height)
	: SolarPowerletv(SolarPowerVStatus::Normal, width, height) {}

SolarPowerletv::SolarPowerletv(SolarPowerVStatus default_status, float width, float height)
	: Svglet(default_status, width, height) {}

Platform::String^ SolarPowerletv::name() {
	return "SolarPowerPanel";
}

void SolarPowerletv::prepare_style(SolarPowerVStatus status, SolarPowerVStyle& style) {
	CAS_SLOT(style.border_color, Colours::Firebrick);
}

void SolarPowerletv::apply_style(SolarPowerVStyle& style) {
	this->set_stroke_color("border", style.border_color);
}
