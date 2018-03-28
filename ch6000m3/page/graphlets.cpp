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
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class Stage final {
public:
	Stage(GraphletOverview* master) : master(master) {
		this->font = make_text_format(16.0F);

		this->subfont = make_text_format();
		this->subfont->FontWeight = FontWeights::Bold;
	}

public:
	void load(float width, float height) {
		Platform::String^ all_labels[] = { "hp_state" };
		float unitsize = 32.0F;

		for (PumpState s = static_cast<PumpState>(0); s < PumpState::_; s++) {
			unsigned int idx = static_cast<unsigned int>(s);
			
			this->pumps[idx] = new Pumplet(s, unitsize);
			this->pump_labels[idx] = new Labellet(this->subfont, speak(s.ToString()));
			this->master->insert(this->pumps[idx]);
			this->master->insert(this->pump_labels[idx]);
		}

		for (size_t i = 0; i < std::min(sizeof(all_labels) / sizeof(Platform::String^), GRAPHLETS_ARITY(this->labels)); i++) {
			this->labels[i] = new Labellet(this->font, speak(all_labels[i]) + ":");
			this->master->insert(this->labels[i]);
		}
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float offset = vinset * 0.5F;
		float x0 = offset;
		float y0 = vinset + offset;

		for (size_t i = 0; i < GRAPHLETS_ARITY(this->labels); i++) {
			if (this->labels[i] != nullptr) {
				float label_width;

				this->labels[i]->fill_extent(0.0F, 0.0F, &label_width);
				label_max_width = fmax(label_max_width, label_width);
			}
		}

		this->pumps[0]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;

		for (size_t i = 0; i < GRAPHLETS_ARITY(this->labels); i++) {
			if (this->labels[i] != nullptr) {
				float y = y0 + halfunit + float(i) * cellsize;

				this->master->move_to(this->labels[i], x0 + label_max_width, y, GraphletAlignment::RC);
			}
		}

		for (size_t i = 0; i < GRAPHLETS_ARITY(this->pumps); i++) {
			float x = x0 + label_max_width + offset + halfunit + float(i) * cellsize;

			this->master->move_to(this->pumps[i], x, y0 + unitsize, GraphletAlignment::CB);
			this->master->move_to(this->pump_labels[i], x, y0 + unitsize, GraphletAlignment::CT);
		}
	}

private: // never delete these graphlets manually.
	Labellet* labels[5];
	Pumplet* pumps[static_cast<unsigned long long>(PumpState::_)];
	Labellet* pump_labels[static_cast<unsigned long long>(PumpState::_)];

private:
	GraphletOverview* master;
	CanvasTextFormat^ font;
	CanvasTextFormat^ subfont;
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

		this->move_to(this->statusline, 0.0F, height, GraphletAlignment::LB);
		
		stage->reflow(width, height, vinset);
		stage->reflow(width, height, vinset);
	}
}
