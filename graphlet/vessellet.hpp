#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private ref class IVessel abstract {
	public:
		virtual void fill_boundary(double* x = nullptr, double* y = nullptr, double* width = nullptr, double* height = nullptr) = 0;
	};

	private class IVessellet abstract : public virtual WarGrey::SCADA::IGraphlet {

	};
}
