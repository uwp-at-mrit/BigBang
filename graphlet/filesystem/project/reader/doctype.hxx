#pragma once

// for concrete doctypes
#include <iostream>
#include <fstream>

namespace WarGrey::SCADA {
	private enum class ProjectDoctype {
		Map_LOG, Depth_LOG,
		DIG, XYZ, MTX,

		Traceline,
		_
	};

	private ref class ProjectDocument abstract {
	public:
		static WarGrey::SCADA::ProjectDocument^ load(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type);

	public:
		virtual ~ProjectDocument() {}
	};
}