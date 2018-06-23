#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class StorageCellVStatus {
		Normal, Breakdown,
		_
	};

	private struct StorageCellVStyle {
		WarGrey::SCADA::Colour^ border_color;
		WarGrey::SCADA::Colour^ body_color;
	};

	WarGrey::SCADA::StorageCellVStyle make_default_storagecellv_style(WarGrey::SCADA::StorageCellVStatus statuss);

	private class StorageCelletv : public WarGrey::SCADA::Svglet<WarGrey::SCADA::StorageCellVStatus, WarGrey::SCADA::StorageCellVStyle> {
	public:
		StorageCelletv(WarGrey::SCADA::StorageCellVStatus default_status, float width = 0.0F, float height = 0.0F);
		StorageCelletv(float width = 0.0F, float height = 0.0F);

	public:
		Platform::String^ name() override;

	protected:
		void apply_style(StorageCellVStyle& style) override;

	private:
		void set_body_color(WarGrey::SCADA::Colour^ color, WarGrey::SCADA::Colour^ shadow = nullptr);
		void set_seal_color(WarGrey::SCADA::Colour^ color);
		void set_anode_color(WarGrey::SCADA::Colour^ color);
		void set_cathode_color(WarGrey::SCADA::Colour^ color);
	};
}
