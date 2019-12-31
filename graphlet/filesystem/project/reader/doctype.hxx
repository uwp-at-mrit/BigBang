#pragma once

// for concrete doctypes
#include <iostream>
#include <fstream>

namespace WarGrey::DTPM {
	private enum class ProjectDoctype {
		Map_LOG, Depth_LOG, Section_LOG,
		DIG, XYZ, MTX, SEC,

		Traceline,
		_
	};

	private ref class ProjectDocument abstract {
	public:
		static WarGrey::DTPM::ProjectDocument^ load(Platform::String^ filename, WarGrey::DTPM::ProjectDoctype type);

	public:
		virtual ~ProjectDocument() {}
	};
}
