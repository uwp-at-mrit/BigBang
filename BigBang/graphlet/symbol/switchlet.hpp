#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class SwitchStatus { Normal, Breakdown, _ };

	private struct SwitchStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	WarGrey::SCADA::SwitchStyle make_default_switch_style(WarGrey::SCADA::SwitchStatus status);

	private class Switchlet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::SwitchStatus, WarGrey::SCADA::SwitchStyle>
		, public WarGrey::SCADA::IValuelet<bool> {
	public:
		Switchlet(WarGrey::SCADA::SwitchStatus default_status, float radius, float thickness = 1.5F, double degrees = 0.0);
		Switchlet(float radius, float thickness = 1.5F, double degrees = -90.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoints;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		float thickness;
		float size;

	private:
		float left_x;
		float left_y;
		float right_x;
		float right_y;
		float handle_x;
		float handle_y;
	};
}
