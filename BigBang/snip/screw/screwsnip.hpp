#pragma once

#include "snip/snip.hpp"
#include "snip/misc.hpp"

namespace WarGrey::SCADA {
    private class IMotorSnip : public WarGrey::SCADA::IPipeSnip {
	public:
		virtual Windows::Foundation::Rect get_motor_port() = 0;
	};

    void pipe_connecting_position(
		WarGrey::SCADA::IPipeSnip* prev, WarGrey::SCADA::IPipeSnip* pipe,
		float* x, float* y, double factor_x = 0.5, double factor_y = 0.5);

    Windows::Foundation::Numerics::float2 pipe_connecting_position(
        WarGrey::SCADA::IPipeSnip* prev, WarGrey::SCADA::IPipeSnip* pipe,
		float x = 0.0F, float y = 0.0F, double factor_x = 0.5, double factor_y = 0.5);
}
