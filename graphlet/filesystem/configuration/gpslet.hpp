#pragma once

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/primitive.hpp"

#include "datum/flonum.hpp"

#include "cs/wgs_xy.hpp"

namespace WarGrey::DTPM {
	private ref class GPSCS sealed {
	public:
		static WarGrey::DTPM::GPSCS^ load(Platform::String^ path);
		static bool save(WarGrey::DTPM::GPSCS^ self, Platform::String^ path);

	public:
		GPSCS(GPSCS^ src = nullptr);
		
	public:
		void refresh(GPSCS^ src);

	internal:
		WarGrey::DTPM::GCSParameter parameter;
	};

	private struct GPSStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ N_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ metrics_font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ arrow_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ arrow_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ N_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ N_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ metrics_color;

		float border_thickness = -1.0F;
		float arrow_border_thickness = -1.0F;
		float N_border_thickness = -1.0F;
		int metrics_precision = -1;
	};

	WarGrey::DTPM::GPSStyle default_gps_style(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ metrics_color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ arrow_color = nullptr, double arrow_alpha = 0.64,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr, double color_alpha = 0.48);

	private class GPSlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::DTPM::GPSCS, WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~GPSlet() noexcept;
		GPSlet(Platform::String^ gps, float radius, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		GPSlet(WarGrey::DTPM::GPSStyle& style, Platform::String^ gps, float radius, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		
	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		void set_north(double degrees, bool force = false);
		void set_speed(double knot, bool force = false);

	public:
		WarGrey::DTPM::GPSCS^ clone_gpscs(WarGrey::DTPM::GPSCS^ dest = nullptr);
		void refresh(GPSCS^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ gps, WarGrey::DTPM::GPSCS^ gps_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ arrow;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ N;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ knot;
		WarGrey::DTPM::GPSStyle style;

	private:
		WarGrey::DTPM::GPSCS^ gps_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		float radius;
		float arrow_radius;
		double degrees;
		double speed;
	};
}
