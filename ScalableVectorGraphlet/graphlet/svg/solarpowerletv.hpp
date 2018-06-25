#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class SolarPowerVStatus { Normal, Breakdown, _ };

	private struct SolarPowerVStyle {
		WarGrey::SCADA::Colour^ border_color;
	};

	private class SolarPowerletv : public WarGrey::SCADA::Svglet<WarGrey::SCADA::SolarPowerVStatus, WarGrey::SCADA::SolarPowerVStyle> {
	public:
		SolarPowerletv(WarGrey::SCADA::SolarPowerVStatus default_status, float width = 0.0F, float height = 0.0F);
		SolarPowerletv(float width = 0.0F, float height = 0.0F);

	public:
		Platform::String^ name() override;

	protected:
		void prepare_style(WarGrey::SCADA::SolarPowerVStatus status, WarGrey::SCADA::SolarPowerVStyle& style);
		void apply_style(WarGrey::SCADA::SolarPowerVStyle& style) override;
	};
}
