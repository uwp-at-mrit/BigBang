#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class WinchStatus {
		WindingUp, FastWindingUp, WindingOut, FastWindingOut,
		WindUpReady, FastWindUpReady, WindOutReady, FastWindOutReady,
		UpperLimited, SoftUpperLimited, LowerLimited, SoftLowerLimited,
		SpoolLimited, SuctionLimited, Unpullable, Unlettable,
		Slack,
		_
	};

	private struct WinchStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ remote_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cable_top_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cable_bottom_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cable_color;
	};

	private class Winchlet
		: public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::WinchStatus, WarGrey::SCADA::WinchStyle> {
	public:
		Winchlet(WarGrey::SCADA::WinchStatus default_status, float width, float height = 0.0F,
			float thickness = 2.0F, unsigned int strand = 8);

		Winchlet(float width, float height = 0.0F, float thickness = 2.0F, unsigned int strand = 8);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_remote_control(bool on);

	protected:
		void prepare_style(WarGrey::SCADA::WinchStatus status, WarGrey::SCADA::WinchStyle& style) override;
		void on_status_changed(WarGrey::SCADA::WinchStatus status) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ icons[_N(WarGrey::SCADA::WinchStatus)];
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ base;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ cable_upper;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ cable_bottom;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cable_base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cable;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cable_style;
		float motion_step;

	private:
		unsigned int strand;
		float icon_cx;
		float icon_cy;
		float width;
		float height;
		float thickness;
		float base_thickness;
		float cable_thickness;

	private:
		bool remote_control;
	};
}