#pragma once

#include "graphlet/svglet.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class StorageCellVStatus {
		Normal, Breakdown,
		_
	};

	private struct StorageCellVStyle {
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
	};
}
