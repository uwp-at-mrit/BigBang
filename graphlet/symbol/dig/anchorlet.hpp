#include <fstream>

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private struct AnchorDig : public IDigDatum {
	public:
		AnchorDig(std::filebuf& dig);
	};
}
