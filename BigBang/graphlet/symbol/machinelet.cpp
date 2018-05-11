#include "graphlet/symbol/machinelet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static MachineStatus default_switch_status = MachineStatus::Normal;
static CanvasSolidColorBrush^ default_color = Colours::GhostWhite;

MachineStyle WarGrey::SCADA::make_default_machine_style(MachineStatus status) {
	MachineStyle s;

	s.border_color = default_color;

	switch (status) {
	case MachineStatus::Breakdown: s.border_color = Colours::Firebrick; break;
	}

	return s;
}

/*************************************************************************************************/
Machinelet::Machinelet(float radius, float thickness, double degrees)
	: Machinelet(default_switch_status, radius, thickness, degrees) {
}

Machinelet::Machinelet(MachineStatus default_status, float radius, float thickness, double degrees)
	: IStatuslet(default_status, &make_default_machine_style)
	, thickness(thickness), size(radius * 2.0F), degrees(degrees) {}

void Machinelet::construct() {
	float epradius = this->thickness;
	float hradius = this->size * 0.5F - epradius;
}

void Machinelet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double Machinelet::get_direction_degrees() {
	return this->degrees;
}

void Machinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const MachineStyle style = this->get_style();
	auto color = (style.border_color != nullptr) ? style.border_color : default_color;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;
}
