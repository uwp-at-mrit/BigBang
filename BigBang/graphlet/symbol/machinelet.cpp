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
IMachinelet::IMachinelet(unsigned char label, float radius, float thickness, double degrees)
	: IMachinelet(default_switch_status, label, radius, thickness, degrees) {
}

IMachinelet::IMachinelet(MachineStatus default_status, unsigned char label, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_machine_style, radius, degrees)
	, thickness(thickness) {}

void IMachinelet::construct() {
	float epradius = this->thickness;
	float hradius = this->size * 0.5F - epradius;
}

void IMachinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const MachineStyle style = this->get_style();
	auto color = (style.border_color != nullptr) ? style.border_color : default_color;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;
}
