#include <unordered_map>

#include "page/graphlets.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"

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
		this->font->FontWeight = FontWeights::Bold;
	}

public:
	void load(float width, float height) {
		Platform::String^ all_labels[] = { "hp_state", "v_state" };
		float unitsize = 32.0F;

		for (size_t i = 0; i < sizeof(all_labels) / sizeof(Platform::String^); i++) {
			this->labels[i] = make_label(speak(all_labels[i]) + ":", this->font);
		}

		for (PumpState s = static_cast<PumpState>(0); s < PumpState::_; s++) {
			unsigned int idx = static_cast<unsigned int>(s);
			
			this->pumps[idx] = make_pump(s, unitsize);
			this->hplabels[idx] = make_label(speak(s.ToString()));
		}

		for (ValveState s = static_cast<ValveState>(0); s < ValveState::_; s++) {
			unsigned int idx = static_cast<unsigned int>(s);

			this->valves[idx] = make_valve(s, unitsize);
			this->vlabels[idx] = make_label(speak(s.ToString()));
		}
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float offset = vinset * 0.5F;
		float x0 = offset;
		float y0 = vinset + offset;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->labels); i++) {
			if (this->labels[i] != nullptr) {
				float label_width;

				this->labels[i]->fill_extent(0.0F, 0.0F, &label_width);
				label_max_width = fmax(label_max_width, label_width);
			}
		}

		this->pumps[0]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->labels); i++) {
			if (this->labels[i] != nullptr) {
				float y = y0 + halfunit + float(i) * cellsize;

				this->master->move_to(this->labels[i], x0 + label_max_width, y, GraphletAlignment::RC);
			}
		}

		x0 += (label_max_width + offset + halfunit);
		y0 += unitsize;
		this->reflow_graphlets(this->pumps, this->hplabels, x0, y0 + cellsize * 0.0F, cellsize, GRAPHLETS_LENGTH(this->pumps));
		this->reflow_graphlets(this->valves, this->vlabels, x0, y0 + cellsize * 1.0F, cellsize, GRAPHLETS_LENGTH(this->valves));
	}

private:
	Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
		Labellet* label = ((font == nullptr) ? new Labellet(text) : new Labellet(font, text));

		this->master->insert(label);

		return label;
	}

	Pumplet* make_pump(PumpState s, float unitsize) {
		Pumplet* pump = new Pumplet(s, unitsize);

		this->master->insert(pump);

		return pump;
	}


	Valvelet* make_valve(ValveState s, float unitsize) {
		Valvelet* valve = new Valvelet(s, unitsize);

		this->master->insert(valve);

		return valve;
	}

private:
	template<typename T>
	void reflow_graphlets(T* gs[], Labellet* ls[], float x0, float y, float cellsize, size_t size) {
		for (size_t i = 0; i < size; i++) {
			float x = x0 + float(i) * cellsize;

			this->master->move_to(gs[i], x, y, GraphletAlignment::CB);
			this->master->move_to(ls[i], x, y, GraphletAlignment::CT);
		}
	}

private: // never delete these graphlets manually.
	Labellet* labels[5];
	Pumplet* pumps[static_cast<unsigned long long>(PumpState::_)];
	Labellet* hplabels[static_cast<unsigned long long>(PumpState::_)];
	Valvelet* valves[static_cast<unsigned long long>(ValveState::_)];
	Labellet* vlabels[static_cast<unsigned long long>(ValveState::_)];

private:
	GraphletOverview* master;
	CanvasTextFormat^ font;
};

/*************************************************************************************************/
static std::unordered_map<GraphletOverview*, Stage*> stages;

GraphletOverview::GraphletOverview() : Planet(":gview:") {
	this->set_decorator(new PageDecorator(Colours::GrayText));
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

		{ // load graphlets
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
