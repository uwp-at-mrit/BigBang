#include <fstream>

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private struct LightHouseDig : public IDigDatum {
	public:
		LightHouseDig(std::filebuf& dig);
	};
}
