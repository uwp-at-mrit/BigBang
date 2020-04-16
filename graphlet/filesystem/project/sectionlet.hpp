#pragma once

#include <utility>

#include "graphlet/vessellet.hpp"
#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private class Sectionlet : public virtual WarGrey::DTPM::MapObjectlet<WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~Sectionlet() noexcept;
		Sectionlet(WarGrey::DTPM::SecDoc^ sec, bool draw_slope_lines = false, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ section_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		const WarGrey::DTPM::Outline* section(double x, double y);
		void merge(WarGrey::DTPM::SecDoc^ sec);

	private:
		void section(double x, double y, double center_x, double center_y);
		void construct_plane();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ slope_style;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ section_color;

	private:
		WarGrey::DTPM::SecDoc^ doc_sec;
		std::deque<std::deque<std::pair<WarGrey::SCADA::double3, WarGrey::SCADA::double3>>> slope_segments;

	private:
		float thickness;
		bool draw_slope_lines;

	private:
		WarGrey::DTPM::Outline* plane;
		WarGrey::SCADA::double2 ps_boundry;
		WarGrey::SCADA::double2 sb_boundry;
	};
}
