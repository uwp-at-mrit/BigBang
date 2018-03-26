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
	Stage(Graphlets* master) : stagebench(master) {}

public:
	void load(float width, float height) {}
	void reflow(float width, float height, float vinset) {}

private:
	Graphlets * stagebench;
};

/*************************************************************************************************/
static std::unordered_map<Graphlets*, Stage*> stages;

Graphlets::Graphlets() : Planet(":gview:") {
	this->set_decorator(new PageDecorator(system_graytext_brush()));
}

Graphlets::~Graphlets() {
	auto maybe_stage = stages.find(this);

	if (maybe_stage != stages.end()) {
		delete maybe_stage->second;

		stages.erase(maybe_stage);
	}
}

void Graphlets::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (stages.find(this) == stages.end()) {
		Stage* stage = new Stage(this);
		
		stages.insert(std::pair<Graphlets*, Stage*>(this, stage));

		{ // load snips
			stage->load(width, height);

			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name());
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}
	}
}

void Graphlets::reflow(float width, float height) {
	auto maybe_stage = stages.find(this);
	
	if (maybe_stage != stages.end()) {
		Stage* stage = maybe_stage->second;
		float vinset = statusbar_height();

		this->move_to(this->statusline, 0.0F, height, SnipCenterPoint::LB);
		
		stage->reflow(width, height, vinset);
		stage->reflow(width, height, vinset);
	}
}
