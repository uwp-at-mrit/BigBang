#pragma once

#include <utility>

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private class Sectionlet : public WarGrey::SCADA::IGraphlet {
	public:
		Sectionlet(WarGrey::SCADA::SecDoc^ sec, bool draw_slope_lines = false, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ slope_style;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::SecDoc^ doc_sec;
		std::deque<std::deque<std::pair<WarGrey::SCADA::double3, WarGrey::SCADA::double3>>> slope_segments;

	private:
		float thickness;
		bool draw_slope_lines;
	};
}
