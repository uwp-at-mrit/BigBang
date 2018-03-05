#include "snip.hpp"
#include "planet.hpp"
#include "shape.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

Rect snip_enclosing_box(ISnip* snip, float x, float y, float3x2 transform) {
	float width, height;

	snip->fill_extent(x, y, &width, &height);

	return rectangle(x, y, width, height)->ComputeBounds(transform);
}

void WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx, double fy) {
	Rect in = pipe->get_input_port();
	Rect out = prev->get_output_port();

	float delta_x = (out.X - in.X) + (out.Width - in.Width) * float(fx);
	float delta_y = (out.Y - in.Y) + (out.Height - in.Height) * float(fy);

	if (x != nullptr) { (*x) = (*x) + delta_x; }
	if (y != nullptr) { (*y) = (*y) + delta_y; }
}

float2 WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float x0, float y0, double fx, double fy) {
	float x = x0;
	float y = y0;

	pipe_connecting_position(prev, pipe, &x, &y, fx, fy);

	return float2{ x, y };
}

/*************************************************************************************************/

ISnip::~ISnip() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
}

Syslog* ISnip::get_logger() {
	Syslog* logger = nullptr;

	if (this->info != nullptr) {
		logger = this->info->master->get_logger();
	}

	return logger;
}
