#pragma once

#include <fstream>

namespace WarGrey::SCADA {
	private enum class DigDatumType {
		SunkenShip, LightShip, OilWell, PilotStation, ReportPoint, LightHouse, RedFlag,
		Hoisptal, Tree, Anchor, Chimney, WaterTower, RadarReflector, IslandReef,
		Aquatic, Buoy, TideStation, Kettle, Light, Seamark, Picket, Rock, Text, Number, FishNet, Wreck,
		Typhoon, Compass,

		Arc, Circle, Rectangle, FontText,
		_
	};

	private struct IDigDatum abstract {
	protected:
		IDigDatum(WarGrey::SCADA::DigDatumType type) : type(type), name(nullptr) {}

	public:
		virtual void fill_polar_extent(double* cx, double* cy, double* radius);
		virtual Platform::String^ to_string();

	public:
		WarGrey::SCADA::DigDatumType type;
		Platform::String^ name;
		double x;
		double y;
	};

	private struct IconDig : public WarGrey::SCADA::IDigDatum {
	public:
		IconDig(std::filebuf& dig, WarGrey::SCADA::DigDatumType type);
	};

	private struct CompassDig : public WarGrey::SCADA::IDigDatum {
	public:
		CompassDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		long long color;
	};

	private struct ArcDig : public WarGrey::SCADA::IDigDatum {
	public:
		ArcDig(std::filebuf& dig);

	public:
		void fill_polar_extent(double* cx, double* cy, double* radius) override;
		Platform::String^ to_string() override;

	public:
		double start_degree;
		double stop_degree;
		double radius;
		long long style;
		long long color;
	};

	private struct CircleDig : public WarGrey::SCADA::IDigDatum {
	public:
		CircleDig(std::filebuf& dig);

	public:
		void fill_polar_extent(double* cx, double* cy, double* radius) override;
		Platform::String^ to_string() override;

	public:
		double radius;
		long long style;
		long long color;
		long long filled;
		long long fill_color;
	};

	private struct RectangleDig : public WarGrey::SCADA::IDigDatum {
	public:
		RectangleDig(std::filebuf& dig);

	public:
		void fill_polar_extent(double* cx, double* cy, double* radius) override;
		Platform::String^ to_string() override;

	public:
		double width;
		double height;
		double rotation;
		long long style;
		long long color;
		long long pen_width;
	};

	private struct FontTextDig : public WarGrey::SCADA::IDigDatum {
	public:
		FontTextDig(std::filebuf& dig);

	public:
		void fill_polar_extent(double* cx, double* cy, double* radius) override;
		Platform::String^ to_string() override;

	public:
		double rotation;
		long long color;
		long long font_size;
		long long style;
		long long width;
		Platform::String^ font_name;
	};

	WarGrey::SCADA::IDigDatum* read_dig(std::filebuf& dig);
}
