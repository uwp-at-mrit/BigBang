#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class SolarPowerVStatus {
		Normal, Breakdown,
		_
	};

	private struct SolarPowerVStyle {
		WarGrey::SCADA::Colour^ border_color;
	};

	WarGrey::SCADA::SolarPowerVStyle make_default_solarpowerv_style(WarGrey::SCADA::SolarPowerVStatus statuss);

	private class SolarPowerletv : public WarGrey::SCADA::Svglet<WarGrey::SCADA::SolarPowerVStatus, WarGrey::SCADA::SolarPowerVStyle> {
	public:
		SolarPowerletv(WarGrey::SCADA::SolarPowerVStatus default_status, float width = 0.0F, float height = 0.0F);
		SolarPowerletv(float width = 0.0F, float height = 0.0F);

	public:
		Platform::String^ name() override;

	protected:
		void apply_style(SolarPowerVStyle& style) override;
	};
}
