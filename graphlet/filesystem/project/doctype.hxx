#pragma once

// for concrete doctypes
#include <iostream>
#include <fstream>

namespace WarGrey::SCADA {
	private enum class ProjectDoctype { DIG, _ };

	private ref class ProjectDocument abstract {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::ProjectDocument^>^ load_async(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type);

	public:
		virtual ~ProjectDocument() {}
	};
}
