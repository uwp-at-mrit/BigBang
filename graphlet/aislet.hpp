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

	public:
		AISPositionReport(double lat = WarGrey::SCADA::flnan, double lon = WarGrey::SCADA::flnan);

	public:
		WarGrey::DTPM::AISPositionReport& operator=(const WarGrey::DTPM::AISPositionReport& pr);
	};

	private struct AISVoyageReport {
		std::string callsign;
		std::string shipname;

		int mothership_mmsi;
		double length;
		double width;
		double gps_fl;
		double gps_fw;

	public:
		AISVoyageReport(std::string shipname = "", std::string callsign = "");

	public:
		WarGrey::DTPM::AISVoyageReport& operator=(const WarGrey::DTPM::AISVoyageReport& pr);
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
		void update_self_position(WarGrey::DTPM::AISPositionReport* pr);
		void update_position(uint16 mmsi, WarGrey::DTPM::AISPositionReport* pr);
		void update_voyage(uint16 mmsi, WarGrey::DTPM::AISVoyageReport* vr, bool force_update = false);

	private:
		WarGrey::DTPM::AISPositionReport self_position;
		std::map<uint16, WarGrey::DTPM::AISPositionReport> positions;
		std::map<uint16, WarGrey::DTPM::AISVoyageReport> voyages;

	private:
		float width;
		float height;
	};
}
