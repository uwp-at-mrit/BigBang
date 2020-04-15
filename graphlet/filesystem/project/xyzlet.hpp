#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

#include "graphlet/filesystem/configuration/colorplotlet.hpp"

namespace WarGrey::DTPM {
	private class Xyzlet : public WarGrey::DTPM::MapObjectlet<WarGrey::SCADA::IGraphlet> {
	public:
		Xyzlet(WarGrey::DTPM::XyzDoc^ depths, float diff_ft_times = 1.6F);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_color_schema(WarGrey::DTPM::ColorPlotlet* plot, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback = nullptr);

	protected:
		void on_map_updated() override;

	private:
		void find_or_create_depth_geometry(double depth, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ gs[], float* whole_width);

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ default_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ location;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> wholes;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> fractions;

	private:
		WarGrey::DTPM::XyzDoc^ doc_xyz;
		WarGrey::DTPM::ColorPlotlet* plot;
		float diff_multiple;

	private:
		float loc_xoff;
		float loc_yoff;
		float num_size;
	};
}
