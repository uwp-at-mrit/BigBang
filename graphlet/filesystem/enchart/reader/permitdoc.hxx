#pragma once

#include <deque>
#include <map>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private enum class PermitContent { Full, Partial, _ };
	private enum class PermitServiceLevel { Subscription, SinglePurchase, _ };

	private struct ENCell {
		std::string permit;
		WarGrey::SCADA::PermitServiceLevel type;
		unsigned long long edition;
		std::string data_server_id;
		Platform::String^ comment;
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
