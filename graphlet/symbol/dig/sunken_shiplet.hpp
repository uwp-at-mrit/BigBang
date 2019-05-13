#include <fstream>

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private struct SunkenShipDig : public IDigDatum {
	public:
		SunkenShipDig(std::filebuf& dig);
	};
}
