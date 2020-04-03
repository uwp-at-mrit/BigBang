#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"

namespace WarGrey::DTPM {
	private class AISlet : public WarGrey::SCADA::IGraphlet {
	public:
		AISlet();

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		bool is_colliding_with_mouse(float local_x, float local_y) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void on_tap(float local_x, float local_y) override;
		
	public:
		void attach_to_map(WarGrey::DTPM::DigMaplet* master, bool force = false);
		void on_vessel_move(double vessel_x, double vessel_y);

	private:
		WarGrey::DTPM::DigMaplet* master;
	};
}
