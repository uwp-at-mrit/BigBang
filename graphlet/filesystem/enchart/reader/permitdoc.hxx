#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::SCADA {
	private enum class PermitContent { Full, Partial, _ };
	private enum class PermitServiceLevel { Subscription, SinglePurchase, _ };

	private struct ENCell {
	public:
		char name[9];
		int expiry_year;
		int expiry_month;
		int expiry_day;
		WarGrey::SCADA::Natural ECK1;
		WarGrey::SCADA::Natural ECK2;
		WarGrey::SCADA::Natural checksum;

	public:
		WarGrey::SCADA::Natural key1;
		WarGrey::SCADA::Natural key2;

	public:
		WarGrey::SCADA::PermitServiceLevel type;
		unsigned long long edition; // deprecated
		bytes data_server_id;
		Platform::String^ comment;

	public:
		bool malformed();
	};

	private ref class PermitDoc sealed : public WarGrey::SCADA::ENChartDocument {
	internal:
		PermitDoc(std::filebuf& dig);

	internal:
		unsigned int cdate;
		unsigned char chour;
		unsigned char cminute;
		unsigned char csecond;
		unsigned char version;
		WarGrey::SCADA::PermitContent content;

	internal:
		std::deque<WarGrey::SCADA::ENCell> encs;
		std::deque<WarGrey::SCADA::ENCell> ecss;

	private:
		void fill_cell_permit(std::filebuf& dig, WarGrey::SCADA::ENCell* cell);
	};
}
