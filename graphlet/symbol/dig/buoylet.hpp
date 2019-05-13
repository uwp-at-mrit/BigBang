#include <fstream>

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private struct BuoyDig : public IconDig {
	public:
		BuoyDig(std::filebuf& dig, char subtype);

	public:
		Platform::String^ to_string() override;

	public:
		char subtype;
	};
}
