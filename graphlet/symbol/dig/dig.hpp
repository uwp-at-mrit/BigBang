#pragma once

#include <fstream>
#include <deque>

namespace WarGrey::SCADA {
	private enum class DigDatumType {
		SunkenShip, LightShip, OilWell, PilotStation, ReportPoint, LightHouse, RedFlag,
		Hoisptal, Tree, Anchor, Chimney, WaterTower, RadarReflector, IslandReef,
		Aquatic, Buoy, TideStation, Kettle, Light, Seamark, Picket, Rock, Text, Number, FishNet, Wreck,
		Typhoon, Compass,

		Arc, Circle, Polyline, Rectangle, Line, FontText,
		_
	};

	private struct IDigDatum abstract {
	protected:
		IDigDatum(WarGrey::SCADA::DigDatumType type) : type(type), name(nullptr) {}

	public:
		virtual void fill_enclosing_box(double* x, double* y, double* width, double* height);
		virtual Platform::String^ to_string();

	public:
		WarGrey::SCADA::DigDatumType type;
		Platform::String^ name;
		double x;
		double y;
	};

	private struct IMultiDigDatum abstract : public WarGrey::SCADA::IDigDatum {
	protected:
		IMultiDigDatum(WarGrey::SCADA::DigDatumType type) : IDigDatum(type) {}

	public:
		virtual void append_line(std::filebuf& src);

	public:
		std::deque<double> rest_xs;
		std::deque<double> rest_ys;
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
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
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
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
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
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
		Platform::String^ to_string() override;

	public:
		double width;
		double height;
		double rotation;
		long long style;
		long long color;
		long long pen_width;
	};

	private struct LineDig : public WarGrey::SCADA::IDigDatum {
	public:
		LineDig(std::filebuf& dig);

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
		Platform::String^ to_string() override;

	public:
		double stop_x;
		double stop_y;
		long long style;
		long long color;
		long long linewidth;
	};

	private struct FontTextDig : public WarGrey::SCADA::IDigDatum {
	public:
		FontTextDig(std::filebuf& dig);

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
		Platform::String^ to_string() override;

	public:
		double rotation;
		long long color;
		long long font_size;
		long long style;
		long long width;
		Platform::String^ font_name;
	};

	private struct PolylineDig : public WarGrey::SCADA::IMultiDigDatum {
	public:
		PolylineDig(std::filebuf& dig);

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
		Platform::String^ to_string() override;

	public:
		long long color;
		long long style;
		long long subtype;
		long long width;
	};

	private struct TyphoonDig : public WarGrey::SCADA::IMultiDigDatum {
	public:
		TyphoonDig(std::filebuf& dig);

	public:
		void append_line(std::filebuf& dig) override;
		void fill_enclosing_box(double* x, double* y, double* width, double* height) override;
		Platform::String^ to_string() override;

	public:
		long long style;
		long long linewidth;
		long long color;
		long long radius;

	public:
		std::deque<Platform::String^> datetimes;
		std::deque<double> pressures;
		std::deque<double> max_wind_speeds;
		std::deque<double> move_speeds;
	};

	WarGrey::SCADA::IDigDatum* read_dig(std::filebuf& dig);
}
