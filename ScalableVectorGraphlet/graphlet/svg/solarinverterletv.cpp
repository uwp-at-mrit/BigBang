#include "graphlet/svg/solarinverterletv.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
SolarInverterletv::SolarInverterletv(float width, float height)
	: SolarInverterletv(SolarInverterVStatus::Normal, width, height) {}

SolarInverterletv::SolarInverterletv(SolarInverterVStatus default_status, float width, float height)
	: Svglet(default_status, width, height) {}

Platform::String^ SolarInverterletv::name() {
	return "SolarPowerPanel";
}

void SolarInverterletv::prepare_style(SolarInverterVStatus status, SolarInverterVStyle& s) {
	switch (status) {
	case SolarInverterVStatus::Breakdown: CAS_SLOT(s.border_color, Colours::Firebrick); break;
	}

	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
}

void SolarInverterletv::apply_style(SolarInverterVStyle& style) {
	this->set_fill_color("border", style.border_color);
}
