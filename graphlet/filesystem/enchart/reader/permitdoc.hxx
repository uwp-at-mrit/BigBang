#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private enum class PermitContent { Full, Partial, _ };
	private enum class PermitServiceLevel { Subscription, SinglePurchase, _ };

	private struct ENCell {
	public:
		char name[9];
		int expiry_year;
		int expiry_month;
		int expiry_day;
		WarGrey::GYDM::Natural ECK1;
		WarGrey::GYDM::Natural ECK2;
		WarGrey::GYDM::Natural checksum;

	public:
		WarGrey::GYDM::Natural key1;
		WarGrey::GYDM::Natural key2;

	public:
		WarGrey::DTPM::PermitServiceLevel type;
		unsigned long long edition; // deprecated
		WarGrey::SCADA::bytes data_server_id;
		Platform::String^ comment;

	public:
		bool malformed();
	};

	private ref class PermitDoc sealed : public WarGrey::DTPM::ENChartDocument {
	internal:
		PermitDoc(std::filebuf& dig);

	internal:
		unsigned int cdate;
		unsigned char chour;
		unsigned char cminute;
		unsigned char csecond;
		unsigned char version;
		WarGrey::DTPM::PermitContent content;

	internal:
		std::deque<WarGrey::DTPM::ENCell> encs;
		std::deque<WarGrey::DTPM::ENCell> ecss;

	private:
		void fill_cell_permit(std::filebuf& dig, WarGrey::DTPM::ENCell* cell);
	};
}
