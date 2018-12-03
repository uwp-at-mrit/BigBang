#pragma once

#include "graphlet/dashboard/timeserieslet.hpp"

#include "sqlite3/rotation.hpp"

namespace WarGrey::SCADA {
	private enum class EWTS {
		EarthWork, Vessel, HopperHeight, Loading, Displacement,
		_ 
	};

	Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ earthwork_line_color_dictionary(unsigned int index);

	private class EarthWorkDataSource
		: public WarGrey::SCADA::ITimeSeriesDataSource
		, public WarGrey::SCADA::RotativeSQLite3 {
	public:
		EarthWorkDataSource(WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::RotationPeriod period = RotationPeriod::Daily,
			unsigned int period_count = 1U);

	public:
		bool ready() override;
		void load(WarGrey::SCADA::ITimeSeriesDataReceiver* receiver, long long open_s, long long closed_s) override;
		void save(long long timepoint, double* values, unsigned int n) override;

	protected:
		void on_database_rotated(WarGrey::SCADA::SQLite3* prev_dbc, WarGrey::SCADA::SQLite3* current_dbc) override;

	protected:
		~EarthWorkDataSource() noexcept {}
	};
}
