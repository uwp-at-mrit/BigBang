#pragma once

#include <map>

#include "graphlet/filesystem/project/digmaplet.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private struct AISPositionReport {
		double latitude;
		double longitude;
		double turn;
		double speed;
		double course;
		double heading;
		
		WarGrey::SCADA::double2 geo;
	};

	/*********************************************************************************************/
	private class AISlet : public WarGrey::DTPM::MapObjectlet<WarGrey::SCADA::IGraphlet> {
	public:
		AISlet(float width, float height);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		bool is_colliding_with_mouse(float local_x, float local_y) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void on_tap(float local_x, float local_y) override;
		
	public:
		void update_position(uint16 mmsi, WarGrey::DTPM::AISPositionReport* pr);

	private:
		std::map<uint16, WarGrey::DTPM::AISPositionReport> positions;

	private:
		float width;
		float height;
	};
}
