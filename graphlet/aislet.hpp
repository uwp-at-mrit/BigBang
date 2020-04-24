#pragma once

#include <map>

#include "graphlet/filesystem/project/digmaplet.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private enum class AISType { A, B };

	private struct AISPositionReport {
		WarGrey::DTPM::AISType type;

		double latitude;
		double longitude;
		double turn;
		double speed;
		double course;
		double heading;
		
		WarGrey::SCADA::double2 geo;

	public:
		AISPositionReport(WarGrey::DTPM::AISType type = AISType::A, double lat = WarGrey::SCADA::flnan, double lon = WarGrey::SCADA::flnan);

	public:
		WarGrey::DTPM::AISPositionReport& operator=(const WarGrey::DTPM::AISPositionReport& pr);
	};

	private struct AISVoyageReport {
		std::string callsign;
		std::string shipname;

		int mothership_mmsi;
		double to_bow;
		double to_stern;
		double to_port;
		double to_starboard;

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
		void update_self_position(double geo_x, double geo_y);
		void update_position(uint16 mmsi, WarGrey::DTPM::AISPositionReport* pr);
		void update_voyage(uint16 mmsi, WarGrey::DTPM::AISVoyageReport* vr, bool force_update = false);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ make_ship_info(Platform::String^ name, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font);
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ship_shape(WarGrey::DTPM::AISVoyageReport* voyage);
		
	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ caption_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ distance_font;

	private:
		std::map<uint16, WarGrey::DTPM::AISPositionReport> positions;
		std::map<uint16, WarGrey::DTPM::AISVoyageReport> voyages;
		std::map<uint16, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> vessels;
		std::map<uint16, Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> captions;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ default_vessel;
		
	private:
		float width;
		float height;

	private:
		double geo_x;
		double geo_y;
	};
}
