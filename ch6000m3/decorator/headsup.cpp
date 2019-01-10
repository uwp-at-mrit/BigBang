#include "decorator/headsup.hpp"
#include "configuration.hpp"

#include "module.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

static ICanvasBrush^ brush = Colours::Silver;

private class HeadsUpDecorator : virtual public WarGrey::SCADA::IPlanetDecorator {
public:
	void draw_after_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) override {
		if (x == 0.0) {
			if (y == 0.0F) { // statusbar's bottomline
				ds->DrawLine(0.0F, height, width, height, brush, 2.0F);
			} else { // statusline's topline
				ds->DrawLine(0.0F, y, width, y, brush, 2.0F);
			}
		}
	}

protected:
	virtual ~HeadsUpDecorator() noexcept {}
};

/*************************************************************************************************/
HeadsUpPlanet::HeadsUpPlanet(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	this->push_decorator(new HeadsUpDecorator());
}

void HeadsUpPlanet::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->statusbar == nullptr) {
		this->statusbar = this->insert_one(new Statusbarlet(this->device));
		this->statusline = this->insert_one(new Statuslinelet(default_logging_level));
		
		{ // delayed initializing
			this->get_logger()->push_log_receiver(this->statusline);

			if (this->device != nullptr) {
				this->device->get_logger()->push_log_receiver(this->statusline);
			}
		}
	}
}

void HeadsUpPlanet::on_transfer(IPlanet* from, IPlanet* to) {
	this->statusbar->set_caption(to->display_name());
}

void HeadsUpPlanet::reflow(float width, float height) {
	this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
}

bool HeadsUpPlanet::can_select(IGraphlet* g) {
	return false;
}

bool HeadsUpPlanet::can_select_multiple() {
	return false;
}
