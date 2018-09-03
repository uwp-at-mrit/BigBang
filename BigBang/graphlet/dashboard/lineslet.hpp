#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	typedef Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ (*tell_line_color)(unsigned int idx);

	private enum class LinesStatus { Realtime, Manual, _ };

	private struct TimeSeries {
		long long start;
		long long span;
		unsigned int step;
	};

	TimeSeries make_today_series(unsigned int step = 11);

	private struct LinesStyle {
		WarGrey::SCADA::tell_line_color line_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ haxes_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ vaxes_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ haxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ vaxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ lines_style;

		float lines_thickness = -1.0F;
		float border_thickness = -1.0F;
		float haxes_thickness = -1.0F;
		float vaxes_thickness = -1.0F;
	};

	private class Lineslet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::LinesStatus, WarGrey::SCADA::LinesStyle> {
	public:
		Lineslet(double range, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 2U);

		Lineslet(double range, WarGrey::SCADA::TimeSeries& ts, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 2U);

		Lineslet(double vmin, double vmax, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 2U);

		Lineslet(double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 2U);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::LinesStatus status, WarGrey::SCADA::LinesStyle& style) override;
		void on_status_changed(WarGrey::SCADA::LinesStatus status) override;

	private:
		void construct_vertical_axes();
		void construct_horizontal_axes();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ haxes;

	private:
		float width;
		float height;

	private:
		WarGrey::SCADA::TimeSeries& series;
		unsigned int step;
		unsigned int precision;

	private:
		double vmin;
		double vmax;
	};
}
