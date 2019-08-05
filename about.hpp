#pragma once

#include "satellite.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::ISatellite* make_about(Platform::String^ logo_name,
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bgcolor,
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ fgcolor,
		int start_year = 2018,
		Platform::String^ logo_rootdir = nullptr);
}
