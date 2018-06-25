#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class SolarInverterVStatus { Normal, Breakdown, _ };

	private struct SolarInverterVStyle {
		WarGrey::SCADA::Colour^ border_color;
	};

	private class SolarInverterletv : public WarGrey::SCADA::Svglet<WarGrey::SCADA::SolarInverterVStatus, WarGrey::SCADA::SolarInverterVStyle> {
	public:
		SolarInverterletv(WarGrey::SCADA::SolarInverterVStatus default_status, float width = 0.0F, float height = 0.0F);
		SolarInverterletv(float width = 0.0F, float height = 0.0F);

	public:
		Platform::String^ name() override;

	protected:
		void prepare_style(WarGrey::SCADA::SolarInverterVStatus status, WarGrey::SCADA::SolarInverterVStyle& style);
		void apply_style(WarGrey::SCADA::SolarInverterVStyle& style) override;
	};
}
