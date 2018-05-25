#pragma once

#include <cinttypes>
#include <string>

namespace WarGrey::SCADA {
	private enum class SDT { Integer, Float, Text, Blob };

	typedef int64 Integer;
	typedef double Float;
	typedef Platform::String^ Text;
	typedef std::string Blob;
}
