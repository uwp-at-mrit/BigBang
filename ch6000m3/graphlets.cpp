#include <unordered_map>

#include "graphlets.hpp"
#include "configuration.hpp"

#include "graphlet/device/winchlet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/pump/water_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
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
	winch_1, winch_2,
	hydraulic_pump, hopper_pump, water_pump,
	gate_valve, motor_valve,
	hopperdoor, upperdoor,
	_
};

private class Stage final {
public:
	Stage(GraphletOverview* master) : master(master), progress(0.0) {}

public:
	void load(float width, float height, float vinset) {
		float unitsize = (height - vinset - vinset) / (float(_N(GS)) * 2.0F * 1.618F);

		this->font = make_bold_text_format("Microsoft YaHei", std::fminf(unitsize * 0.5F, 16.0F));
		this->status_font = make_bold_text_format("Microsoft YaHei", 10.0F);
		
		for (GS id = _E(GS, 0); id < GS::_; id++) {
			this->captions[_I(id)] = make_label(_speak(id) + ":", this->font);
		}

		this->load_primitives(this->winchs, this->wlabels, unitsize * 2.0F);
		this->load_primitives(this->pumps, this->plabels, unitsize);
		this->load_primitives(this->hpumps, this->hplabels, unitsize * 0.5F);
		this->load_primitives(this->cpumps, this->cplabels, unitsize);
		this->load_primitives(this->gvalves, this->gvlabels, unitsize);
		this->load_primitives(this->evalves, this->evlabels, unitsize);
		this->load_primitives(this->hdoors, this->hdlabels, unitsize);
		this->load_primitives(this->udoors, this->udlabels, unitsize);
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float x0 = vinset * 0.5F;
		float y = 0.0F;

		for (size_t i = 0; i < _N(GS); i++) {
			if (this->captions[i] != nullptr) {
				float label_width;

				this->captions[i]->fill_extent(0.0F, 0.0F, &label_width);
				label_max_width = fmax(label_max_width, label_width);
			}
		}

		this->pumps[_E0(HydraulicPumpStatus)]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;
		y = (height - cellsize * float(_N(GS))) * 0.5F;

		for (size_t i = 0; i < _N(GS); i++) {
			if (this->captions[i] != nullptr) {
				float yi = y + halfunit + float(i) * cellsize;

				this->master->move_to(this->captions[i], x0 + label_max_width, yi, GraphletAnchor::RC);
			}
		}

		x0 += (label_max_width + x0 + x0 + halfunit);

		{ // split winch statuses
			WinchStatus at = _E(WinchStatus, _N(WinchStatus) / 2);

			this->reflow_primitives(this->winchs, this->wlabels, _E0(WinchStatus), at, x0, &y, halfunit, cellsize);
			this->reflow_primitives(this->winchs, this->wlabels, at, WinchStatus::_,   x0, &y, halfunit, cellsize);
		}

		this->reflow_primitives(this->pumps,  this->plabels,   x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->hpumps, this->hplabels,  x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->cpumps, this->cplabels,  x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->gvalves, this->gvlabels, x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->evalves, this->evlabels, x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->hdoors, this->hdlabels,  x0, &y, halfunit, cellsize);
		this->reflow_primitives(this->udoors, this->udlabels,  x0, &y, halfunit, cellsize);
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
			ls[s] = make_label(_speak(s), this->status_font);
		}
	}

	template<typename T, typename S>
	void reflow_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls
		, float x0, float* y, float halfunit, float cellsize) {
		this->reflow_primitives(gs, ls, _E0(S), S::_, x0, y, halfunit, cellsize);
	}

	template<typename T, typename S>
	void reflow_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls
		, S id0, S idN, float x0, float* y, float halfunit, float cellsize) {
		unsigned int i0 = _I(id0);

		for (S i = id0; i < idN; i++) {
			float x = x0 + float(_I(i) - i0) * cellsize;

			this->master->move_to(gs[i], x, (*y) + halfunit * 1.0F, GraphletAnchor::CC);
			this->master->move_to(ls[i], x, (*y) + halfunit * 2.0F, GraphletAnchor::CT);
		}

		(*y) = (*y) + cellsize;
	}

private: // never delete these graphlets manually.
	Labellet* captions[_N(GS)];
	std::unordered_map<WinchStatus, Winchlet*> winchs;
	std::unordered_map<WinchStatus, Labellet*> wlabels;
	std::unordered_map<HydraulicPumpStatus, HydraulicPumplet*> pumps;
	std::unordered_map<HydraulicPumpStatus, Labellet*> plabels;
	std::unordered_map<HopperPumpStatus, HopperPumplet*> hpumps;
	std::unordered_map<HopperPumpStatus, Labellet*> hplabels;
	std::unordered_map<WaterPumpStatus, WaterPumplet*> cpumps;
	std::unordered_map<WaterPumpStatus, Labellet*> cplabels;
	std::unordered_map<GateValveStatus, GateValvelet*> gvalves;
	std::unordered_map<GateValveStatus, Labellet*> gvlabels;
	std::unordered_map<TValveStatus, MotorValvelet*> evalves;
	std::unordered_map<TValveStatus, Labellet*> evlabels;
	std::unordered_map<DoorStatus, HopperDoorlet*> hdoors;
	std::unordered_map<DoorStatus, Labellet*> hdlabels;
	std::unordered_map<DoorStatus, UpperHopperDoorlet*> udoors;
	std::unordered_map<DoorStatus, Labellet*> udlabels;

private:
	GraphletOverview* master;
	CanvasTextFormat^ font;
	CanvasTextFormat^ status_font;

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

			this->get_logger()->append_log_receiver(this->statusline);
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
