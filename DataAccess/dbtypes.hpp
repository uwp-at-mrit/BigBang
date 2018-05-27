#pragma once

#include <cinttypes>
#include <string>

namespace WarGrey::SCADA {
	private enum class SDT { Integer, Float, Text, Bytes };

	typedef int64 Integer;
	typedef double Float;
	typedef std::string Text;
}
