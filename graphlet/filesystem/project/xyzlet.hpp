#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

namespace WarGrey::SCADA {
	private class Xyzlet : public WarGrey::SCADA::IGraphlet {
	public:
		Xyzlet(WarGrey::SCADA::XyzDoc^ depths, double width, double height, float diff_ft_times = 1.2F);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);

	private:
		void find_or_create_depth_geometry(double depth, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ gs[], float* whole_width);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ location;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> wholes;
		std::map<int, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> fractions;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::XyzDoc^ doc_xyz;
		float diff_multiple;
		float width;
		float height;

	private:
		float loc_xoff;
		float loc_yoff;
		float num_size;
	};
}
