﻿#include <unordered_map>

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
			this->captions[i] = make_label(speak(all_labels[i]) + ":", this->font);
		}

		this->load_primitives<Pumplet, PumpState>(this->pumps, this->hplabels, unitsize);
		this->load_primitives<Valvelet, ValveState>(this->valves, this->vlabels, unitsize);
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float offset = vinset * 0.5F;
		float x0 = offset;
		float y0 = vinset + offset;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				float label_width;

				this->captions[i]->fill_extent(0.0F, 0.0F, &label_width);
				label_max_width = fmax(label_max_width, label_width);
			}
		}

		this->pumps[0]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				float y = y0 + halfunit + float(i) * cellsize;

				this->master->move_to(this->captions[i], x0 + label_max_width, y, GraphletAlignment::RC);
			}
		}

		x0 += (label_max_width + offset + halfunit);
		y0 += unitsize;
		this->reflow_primitives<Pumplet, PumpState>(this->pumps, this->hplabels,   x0, y0 + cellsize * 0.0F, cellsize);
		this->reflow_primitives<Valvelet, ValveState>(this->valves, this->vlabels, x0, y0 + cellsize * 1.0F, cellsize);
	}

private:
	Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
		Labellet* label = ((font == nullptr) ? new Labellet(text) : new Labellet(font, text));

		this->master->insert(label);

		return label;
	}

	template<typename T, typename S>
	void load_primitives(T* gs[], Labellet* ls[], float unitsize) {
		for (S s = static_cast<S>(0); s < S::_; s++) {
			unsigned int idx = static_cast<unsigned int>(s);

			gs[idx] = new T(s, unitsize);
			this->master->insert(gs[idx]);

			ls[idx] = make_label(speak(s.ToString()));
		}
	}

	template<typename T, typename S>
	void reflow_primitives(T* gs[], Labellet* ls[], float x0, float y, float cellsize) {
		for (size_t i = 0; i < static_cast<unsigned int>(S::_); i++) {
			float x = x0 + float(i) * cellsize;

			this->master->move_to(gs[i], x, y, GraphletAlignment::CB);
			this->master->move_to(ls[i], x, y, GraphletAlignment::CT);
		}
	}

private: // never delete these graphlets manually.
	Labellet* captions[5];
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
