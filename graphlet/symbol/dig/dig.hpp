#pragma once

#include <fstream>
#include <deque>

#include "forward.hpp"

#include "datum/object.hpp"

namespace WarGrey::DTPM {
	private enum class DigDatumType {
		Icon, Arc, Circle, Line, PolyLine, PolyBezier, ShoreLine, Rectangle, Text, Number, FontText,

		// TODO
		Typhoon, Compass,
		_
	};
	
	private enum class DigIcon {
		SunkenShip, LightShip, OilWell, PilotStation, ReportSpot, LightHouse, RedFlag,
		Hoisptal, Tree, Anchor, Chimney, WaterTower, RadarReflector, IslandReef,
		Aquatic, Buoy, TideStation, Kettle, Light, NavigationMark, Picket, Rock, FishingFloat, Wreck,
		_
	};

	private struct IDigDatum abstract : public virtual WarGrey::SCADA::SharedObject {
	protected:
		virtual ~IDigDatum() noexcept {}
		IDigDatum(WarGrey::DTPM::DigDatumType type) : type(type), name(nullptr) {}

	public:
		virtual WarGrey::SCADA::IGraphlet* make_graphlet(double* x, double* y);
		virtual Platform::String^ to_string();

	public:
		virtual bool multiline() { return false; }
		virtual void push_line(std::filebuf& src) {}

	public:
		WarGrey::DTPM::DigDatumType type;
		Platform::String^ name;
		double x;
		double y;

	public:
		double lx;
		double ty;
		double rx;
		double by;
	};

	private struct IconDig : public WarGrey::DTPM::IDigDatum {
	public:
		IconDig(std::filebuf& dig, WarGrey::DTPM::DigIcon type, float size);

	public:
		WarGrey::SCADA::IGraphlet* make_graphlet(double* x, double* y) override;

	public:
		WarGrey::DTPM::DigIcon subtype;

	private:
		float size;
	};

	private struct IMultilineDigDatum abstract : public WarGrey::DTPM::IDigDatum {
	protected:
		virtual ~IMultilineDigDatum() noexcept {}
		IMultilineDigDatum(WarGrey::DTPM::DigDatumType type) : IDigDatum(type) {}

	public:
		bool multiline() override;
		void push_line(std::filebuf& src) override;

	public:
		std::deque<double> poly_xs;
		std::deque<double> poly_ys;
	};

	private struct ArcDig : public WarGrey::DTPM::IDigDatum {
	public:
		ArcDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		double start_degree;
		double stop_degree;
		double radius;
		long long style;
		long long color;
	};

	private struct CircleDig : public WarGrey::DTPM::IDigDatum {
	public:
		CircleDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		double radius;
		long long style;
		long long color;
		long long filled;
		long long fill_color;
	};

	private struct RectangleDig : public WarGrey::DTPM::IDigDatum {
	public:
		RectangleDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		double width;
		double height;
		double rotation;
		long long style;
		long long color;
		long long pen_width;
	};

	private struct LineDig : public WarGrey::DTPM::IDigDatum {
	public:
		LineDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		double stop_x;
		double stop_y;
		long long style;
		long long color;
		long long linewidth;
	};

	private struct NumberDig : public WarGrey::DTPM::IDigDatum {
	public:
		NumberDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;
	};

	private struct TextDig : public WarGrey::DTPM::IDigDatum {
	public:
		TextDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;
	};

	private struct FontTextDig : public WarGrey::DTPM::IDigDatum {
	public:
		FontTextDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		double rotation;
		long long color;
		long long font_size;
		long long resizable;
		long long width;
		Platform::String^ font_name;
	};

	private struct ShoreLineDig : public WarGrey::DTPM::IMultilineDigDatum {
	public:
		ShoreLineDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;
	};

	private struct PolyLineDig : public WarGrey::DTPM::IMultilineDigDatum {
	public:
		PolyLineDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		long long color;
		long long style;
		long long subtype;
		long long width;
	};

	private struct PolyBezierDig : public WarGrey::DTPM::IMultilineDigDatum {
	public:
		PolyBezierDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		long long color;
		long long style;
		long long width;
	};

	private struct TyphoonDig : public WarGrey::DTPM::IMultilineDigDatum {
	public:
		TyphoonDig(std::filebuf& dig);

	public:
		void push_line(std::filebuf& dig) override;
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

	private struct CompassDig : public WarGrey::DTPM::IDigDatum {
	public:
		CompassDig(std::filebuf& dig);

	public:
		Platform::String^ to_string() override;

	public:
		long long color;
	};

	WarGrey::DTPM::IDigDatum* read_dig_line(std::filebuf& dig, float icon_size);
}
