#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/jobdoc.hxx"

#include "graphlet/filesystem/configuration/colorplotlet.hpp"

namespace WarGrey::SCADA {
	private class Tracelinelet : public WarGrey::SCADA::IGraphlet {
	public:
		Tracelinelet(WarGrey::SCADA::JobDoc^ jobs, float handler_size = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ hicolor = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ handler_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ handler_hicolor = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		bool is_colliding_with_mouse(float local_x, float local_y) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void on_tap(float local_x, float local_y) override;
		
	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);
		void on_vessel_move(double vessel_x, double vessel_y);

	private:
		int find_handler(float local_x, float local_y, int* group = nullptr);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ handler;

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ hicolor;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ handler_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ handler_hicolor;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::JobDoc^ jobs_dat;
		float handler_half_size;
	};
}
