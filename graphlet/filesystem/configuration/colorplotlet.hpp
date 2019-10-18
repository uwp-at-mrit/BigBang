#pragma once

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/primitive.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
#define ColorPlotSize 20

	private ref class ColorPlot sealed {
	public:
		static WarGrey::SCADA::ColorPlot^ load(Platform::String^ path);
		static bool save(WarGrey::SCADA::ColorPlot^ self, Platform::String^ path);

	public:
		ColorPlot(ColorPlot^ src = nullptr);
		
	public:
		void refresh(ColorPlot^ src);

	internal:
		double depths[ColorPlotSize];
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ colors[ColorPlotSize];
		bool enableds[ColorPlotSize];

	internal:
		double min_depth;
		double max_depth;
	};

	private class ColorPlotlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::ColorPlot, WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~ColorPlotlet() noexcept;
		ColorPlotlet(Platform::String^ plot, float width, float height = 0.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		
	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		bool in_range(double depth);
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ depth_color(double depth,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback);

	public:
		WarGrey::SCADA::ColorPlot^ clone_plot(WarGrey::SCADA::ColorPlot^ dest = nullptr);
		void refresh(ColorPlot^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ plot, WarGrey::SCADA::ColorPlot^ plot_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private:
		WarGrey::SCADA::ColorPlot^ plot_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		float width;
		float height;
	};
}
