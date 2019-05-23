#include <fstream>

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private struct RadarReflectorDig : public IDigDatum {
	public:
		RadarReflectorDig(std::filebuf& dig);
	};
}
