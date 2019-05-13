#pragma once

#include <fstream>

namespace WarGrey::SCADA {
	private enum class DigDatumType {
		SunkenShip, LightShip, OilWell, PilotStation, ReportPoint, LightHouse, RedFlag,
		Hoisptal, Tree, Anchor, Chimney, WaterTower, RadarReflector, IslandReef,
		Aquatic, Buoy, TideStation, Kettle, Light, Seamark, Picket, Rock, Text, Number, FishNet, Wreck,
		Typhoon, Compass,
		_
	};

	private struct IDigDatum abstract {
	protected:
		IDigDatum(WarGrey::SCADA::DigDatumType type) : type(type), name(nullptr) {}

	public:
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

	WarGrey::SCADA::IDigDatum* read_dig(std::filebuf& dig);
}
