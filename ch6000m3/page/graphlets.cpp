#include <unordered_map>

#include "page/graphlets.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"

#include "graphlet/pumplet.hpp"
#include "graphlet/valvelet.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class Stage final {
public:
	Stage(GraphletOverview* master) : master(master) {}

public:
	void load(float width, float height) {
		float unitsize = 32.0F;

		for (PumpState s = static_cast<PumpState>(0); s < PumpState::_; s++) {
			unsigned int idx = static_cast<unsigned int>(s);
			
			this->pumps[idx] = new Pumplet(s, unitsize);
			this->pump_labels[idx] = new Labellet(speak(s.ToString()));
			this->master->insert(this->pumps[idx]);
			this->master->insert(this->pump_labels[idx]);
		}
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, label_width;
		float x0 = vinset;
		float y0 = vinset + vinset;

		this->pumps[0]->fill_extent(0.0F, 0.0F, &unitsize);
		for (size_t i = 0; i < GRAPHLETS_ARITY(this->pumps); i++) {
			float x = x0 + float(i) * unitsize * 1.618F;

			this->pump_labels[i]->fill_extent(x, y0, &label_width);
			this->master->move_to(this->pumps[i], x, y0);
			this->master->move_to(this->pump_labels[i], x + (unitsize - label_width) * 0.5F, y0 + unitsize);
		}
	}

private: // never delete these graphlets manually.
	Pumplet* pumps[static_cast<unsigned long long>(PumpState::_)];
	Labellet* pump_labels[static_cast<unsigned long long>(PumpState::_)];

private:
	GraphletOverview* master;
};

/*************************************************************************************************/
static std::unordered_map<GraphletOverview*, Stage*> stages;

GraphletOverview::GraphletOverview() : Planet(":gview:") {
	this->set_decorator(new PageDecorator(system_graytext_brush()));
}

GraphletOverview::~GraphletOverview() {
	auto maybe_stage = stages.find(this);

	if (maybe_stage != stages.end()) {
		delete maybe_stage->second;

		stages.erase(maybe_stage);
	}
}

void GraphletOverview::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (stages.find(this) == stages.end()) {
		Stage* stage = new Stage(this);
		
		stages.insert(std::pair<GraphletOverview*, Stage*>(this, stage));

		{ // load snips
			stage->load(width, height);

			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name());
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}
	}
}

void GraphletOverview::reflow(float width, float height) {
	auto maybe_stage = stages.find(this);
	
	if (maybe_stage != stages.end()) {
		Stage* stage = maybe_stage->second;
		float vinset = statusbar_height();

		this->move_to(this->statusline, 0.0F, height, SnipCenterPoint::LB);
		
		stage->reflow(width, height, vinset);
		stage->reflow(width, height, vinset);
	}
}
