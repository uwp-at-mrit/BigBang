#include <unordered_map>

#include "page/graphlets.hpp"
#include "configuration.hpp"

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/manual_valvelet.hpp"
#include "graphlet/symbol/valve/tagged_valvelet.hpp"
#include "graphlet/symbol/door/hopper_doorlet.hpp"

#include "decorator/page.hpp"
#include "decorator/margin.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum class GS {
	winch,
	hydraulic_pump, hopper_pump,
	manual_valve, gate_valve, motor_valve,
	hopperdoor, upperdoor,
	_
};

private class Stage final {
public:
	Stage(GraphletOverview* master) : master(master), progress(0.0) {}

public:
	void load(float width, float height, float vinset) {
		float unitsize = (height - vinset - vinset) / (float(_N(GS)) * 2.0F * 1.618F);

		this->font = make_bold_text_format("Microsoft YaHei", std::fminf(unitsize * 0.5F, large_font_size));
		
		for (GS id = _E(GS, 0); id < GS::_; id++) {
			this->captions[_I(id)] = make_label(_speak(id) + ":", this->font);
		}

		this->load_primitives(this->pumps, this->plabels, unitsize);
		this->load_primitives(this->hpumps, this->hplabels, unitsize * 0.5F);
		this->load_primitives(this->mvalves, this->mvlabels, unitsize);
		this->load_primitives(this->gvalves, this->gvlabels, unitsize);
		this->load_primitives(this->evalves, this->evlabels, unitsize);
		this->load_primitives(this->hdoors, this->hdlabels, unitsize);
		this->load_primitives(this->udoors, this->udlabels, unitsize);
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

		this->pumps[_E0(HydraulicPumpStatus)]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				float y = y0 + halfunit + float(i) * cellsize;

				this->master->move_to(this->captions[i], x0 + label_max_width, y, GraphletAnchor::RC);
			}
		}

		x0 += (label_max_width + offset + halfunit);
		this->reflow_primitives(this->pumps,  this->plabels,   x0, y0 + cellsize * 1.0F, halfunit, cellsize);
		this->reflow_primitives(this->hpumps, this->hplabels,  x0, y0 + cellsize * 2.0F, halfunit, cellsize);
		this->reflow_primitives(this->mvalves, this->mvlabels, x0, y0 + cellsize * 3.0F, halfunit, cellsize);
		this->reflow_primitives(this->gvalves, this->gvlabels, x0, y0 + cellsize * 4.0F, halfunit, cellsize);
		this->reflow_primitives(this->evalves, this->evlabels, x0, y0 + cellsize * 5.0F, halfunit, cellsize);
		this->reflow_primitives(this->hdoors, this->hdlabels,  x0, y0 + cellsize * 6.0F, halfunit, cellsize);
		this->reflow_primitives(this->udoors, this->udlabels,  x0, y0 + cellsize * 7.0F, halfunit, cellsize);
	}

public:
	void update(long long count, long long interval, long long uptime) {
		this->progress += 0.1;

		if (this->progress > 1.0) {
			this->progress = 0.0;
		}

		hdoors[DoorStatus::Opening]->set_value(this->progress);
		udoors[DoorStatus::Opening]->set_value(this->progress);
		hdoors[DoorStatus::Closing]->set_value(1.0 - this->progress);
		udoors[DoorStatus::Closing]->set_value(1.0 - this->progress);
	}

private:
	Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
		return this->master->insert_one(new Labellet(text, font));
	}

	template<typename T, typename S>
	void load_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls, float unitsize) {
		for (S s = _E0(S); s < S::_; s++) {
			gs[s] = this->master->insert_one(new T(s, unitsize));
			ls[s] = make_label(_speak(s));
		}
	}

	template<typename T, typename S>
	void reflow_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls
		, float x0, float y, float halfunit, float cellsize) {
		for (S i = _E0(S); i < S::_; i++) {
			float x = x0 + _F(i) * cellsize;

			this->master->move_to(gs[i], x, y + halfunit * 1.0F, GraphletAnchor::CC);
			this->master->move_to(ls[i], x, y + halfunit * 2.0F, GraphletAnchor::CT);
		}
	}

private: // never delete these graphlets manually.
	Labellet* captions[_N(GS)];
	std::unordered_map<HydraulicPumpStatus, HydraulicPumplet*> pumps;
	std::unordered_map<HydraulicPumpStatus, Labellet*> plabels;
	std::unordered_map<HopperPumpStatus, HopperPumplet*> hpumps;
	std::unordered_map<HopperPumpStatus, Labellet*> hplabels;
	std::unordered_map<GateValveStatus, GateValvelet*> gvalves;
	std::unordered_map<GateValveStatus, Labellet*> gvlabels;
	std::unordered_map<ManualValveStatus, ManualValvelet*> mvalves;
	std::unordered_map<ManualValveStatus, Labellet*> mvlabels;
	std::unordered_map<TValveStatus, MotorValvelet*> evalves;
	std::unordered_map<TValveStatus, Labellet*> evlabels;
	std::unordered_map<DoorStatus, HopperDoorlet*> hdoors;
	std::unordered_map<DoorStatus, Labellet*> hdlabels;
	std::unordered_map<DoorStatus, UpperHopperDoorlet*> udoors;
	std::unordered_map<DoorStatus, Labellet*> udlabels;

private:
	GraphletOverview* master;
	CanvasTextFormat^ font;

private:
	double progress;
};

/*************************************************************************************************/
static std::unordered_map<GraphletOverview*, Stage*> stages;

GraphletOverview::GraphletOverview() : Planet(__MODULE__) {
	this->append_decorator(new PageDecorator());
	this->append_decorator(new MarginDecorator(true, true));
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
		float vinset = statusbar_height();
		
		stages.insert(std::pair<GraphletOverview*, Stage*>(this, stage));

		{ // load graphlets
			stage->load(width, height, vinset);

			this->statusline = new Statuslinelet(default_logging_level);
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

		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		stage->reflow(width, height, vinset);
	}
}

void GraphletOverview::on_elapse(long long count, long long interval, long long uptime) {
	auto maybe_stage = stages.find(this);

	if (maybe_stage != stages.end()) {
		maybe_stage->second->update(count, interval, uptime);
	}
}

bool GraphletOverview::can_select(IGraphlet* g) {
	return true;
}
