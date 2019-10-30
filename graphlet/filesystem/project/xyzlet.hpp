#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

#include "graphlet/filesystem/configuration/colorplotlet.hpp"

namespace WarGrey::SCADA {
	private class Xyzlet : public WarGrey::SCADA::IGraphlet {
	public:
		Xyzlet(WarGrey::SCADA::XyzDoc^ depths, float diff_ft_times = 2.4F);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);
		void set_color_schema(WarGrey::SCADA::ColorPlotlet* plot, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback = nullptr);

	private:
		void find_or_create_depth_geometry(double depth, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ gs[], float* whole_width);

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ default_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ location;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> wholes;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> fractions;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::XyzDoc^ doc_xyz;
		WarGrey::SCADA::ColorPlotlet* plot;
		float diff_multiple;

	private:
		float loc_xoff;
		float loc_yoff;
		float num_size;
	};
}
