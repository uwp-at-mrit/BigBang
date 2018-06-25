#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class StorageCellVStatus { Normal, Breakdown, Charge, Discharge, _ };

	private struct StorageCellVStyle {
		WarGrey::SCADA::Colour^ body_color;
		WarGrey::SCADA::Colour^ sign_color;
	};

	private class StorageCelletv : public WarGrey::SCADA::Svglet<WarGrey::SCADA::StorageCellVStatus, WarGrey::SCADA::StorageCellVStyle> {
	public:
		StorageCelletv(WarGrey::SCADA::StorageCellVStatus default_status, float width = 0.0F, float height = 0.0F);
		StorageCelletv(float width = 0.0F, float height = 0.0F);

	public:
		Platform::String^ name() override;

	public:
		void update(long long count, long long interval, long long uptime) override;

	protected:
		void on_ready() override;
		void prepare_style(WarGrey::SCADA::StorageCellVStatus status, WarGrey::SCADA::StorageCellVStyle& style) override;
		void apply_style(WarGrey::SCADA::StorageCellVStyle& style) override;

	private:
		void set_body_color(WarGrey::SCADA::Colour^ color);
		void set_seal_color(WarGrey::SCADA::Colour^ color, WarGrey::SCADA::Colour^ shadow = nullptr);
		void set_anode_color(WarGrey::SCADA::Colour^ color);
		void set_cathode_color(WarGrey::SCADA::Colour^ color);
	};
}
