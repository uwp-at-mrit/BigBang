﻿#include "page/hp_single.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.idl"

#include "graphlet/shapelet.hpp"
#include "graphlet/pumplet.hpp"
#include "graphlet/valvelet.hpp"

#include "decorator/page.hpp"
#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPSMode { WindowUI = 0, View };

private enum class HPS : unsigned int {
	A, B, C, D, E, F, G, H, I, J, K, Y, F001, SQ1, SQ2, SQ3,
	SQa, SQb, SQc, SQd, SQe, SQf, SQg, SQh, SQi, SQj, SQk, SQy,
	Port, Starboard,
	_,
	a, b, c, d, e, f, g, h, i, j, k
};

static HPS pump_ids[] = {
	HPS::A, HPS::B, HPS::G, HPS::H,
	HPS::F, HPS::C, HPS::D, HPS::E,
	HPS::Y, HPS::K,
	HPS::J, HPS::I
};

static HPS valve_ids[] = {
	HPS::SQ1, HPS::SQ2, HPS::SQ3, HPS::SQy, HPS::SQi, HPS::SQj,
	HPS::SQa, HPS::SQb, HPS::SQg, HPS::SQh, HPS::SQk,
	HPS::SQf, HPS::SQc, HPS::SQd, HPS::SQe,
};

private class HPSConsole : public WarGrey::SCADA::ModbusConfirmation {
public:
	HPSConsole(HPSingle* master) : workbench(master) {}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HPS>* pTurtle = new Turtle<HPS>(gridsize, true, HPS::SQ1);

		pTurtle->move_down(4)->turn_down_right()->move_right(10)->turn_right_down();
		pTurtle->move_down(5, HPS::f)->move_right(4, HPS::SQf)->move_right(10, HPS::F)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::c)->move_right(4, HPS::SQc)->move_right(10, HPS::C)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::d)->move_right(4, HPS::SQd)->move_right(10, HPS::D)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::e)->move_right(4, HPS::SQe)->move_right(10, HPS::E)->move_right(4)->move_up(12, HPS::Starboard);
		pTurtle->move_up(19)->turn_up_left()->move_left(32);
		pTurtle->turn_left_down(HPS::f)->move_down(1.5F, HPS::F001)->move_down(1.5F)->jump_back();
		pTurtle->turn_up_left()->move_left(26)->turn_left_down()->move_down(17);
		pTurtle->move_down(5, HPS::a)->move_right(4, HPS::A)->move_right(10, HPS::SQa)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::b)->move_right(4, HPS::B)->move_right(10, HPS::SQb)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::g)->move_right(4, HPS::G)->move_right(10, HPS::SQg)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::h)->move_right(4, HPS::H)->move_right(10, HPS::SQh)->move_right(4)->move_up(12, HPS::Port);
		pTurtle->move_up(5)->turn_up_right()->move_right(8)->turn_right_up();
		pTurtle->move_up(1, HPS::SQ2)->jump_right(8, HPS::SQ3)->move_down()->turn_down_right();
		pTurtle->move_right(8, HPS::k)->move_right(4, HPS::SQk)->move_right(4);
		pTurtle->turn_right_up()->move_up(8, HPS::K)->move_up(5)->jump_back();
		pTurtle->move_up(5, HPS::SQy)->move_up(4, HPS::Y)->move_up(5);

		pTurtle->jump_back(HPS::SQ2)->jump_down(12.8F, HPS::j)->move_down(2, HPS::SQj)->move_down(3, HPS::J)->move_down(2);
		pTurtle->jump_back()->jump_right(8, HPS::i)->move_down(2, HPS::SQi)->move_down(3, HPS::I)->move_down(2);

		this->stations[0] = new Tracklet<HPS>(pTurtle, 1.5F, Colours::Silver);
		this->captions[0] = make_caption(HPS::Port, Colours::DarkKhaki);
		this->captions[1] = make_caption(HPS::Starboard, Colours::DarkKhaki);

		this->workbench->insert_all(this->stations);
		this->workbench->insert_all(this->captions);
	}

	void load_pump_elements(float width, float height, float gridsize) {
		{ // load pumps
			this->load_graphlets(this->pumps, this->plabels, pump_ids, gridsize, 180.0, 0, 4, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, pump_ids, gridsize, 0.000, 4, 4, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, pump_ids, gridsize, -90.0, 8, 2, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, pump_ids, gridsize, 90.00, 10, 2, this->pcaptions);
		}

		{ // load valves
			float adjust_gridsize = gridsize * 1.2F;
			
			this->load_graphlets(this->valves, this->vlabels, valve_ids, adjust_gridsize, 0.000, 0, 6);
			this->load_graphlets(this->valves, this->vlabels, valve_ids, adjust_gridsize, -90.0, 6, 5);
			this->load_graphlets(this->valves, this->vlabels, valve_ids, adjust_gridsize, 90.00, 11, 4);
		}
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float sw, sh, sx, sy;

		this->stations[0]->fill_extent(0.0F, 0.0F, &sw, &sh);
		this->workbench->move_to(this->stations[0], (width - sw) * 0.5F, (height - sh) * 0.5F);
		this->workbench->fill_graphlet_location(this->stations[0], &sx, &sy);

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				this->place_id_element(this->captions[i], sx - gridsize, sy, GraphletAlignment::RB);
			}
		}
	}
	
	void reflow_pump_elements(float width, float height, float gridsize, float vinset) {
		float x0, y0, lbl_dx, lbl_dy, cpt_dx, cpt_dy;
		float adjust_offset = gridsize * 0.8F;
		GraphletAlignment lbl_align, cpt_align;

		this->workbench->fill_graphlet_location(this->stations[0], &x0, &y0);

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->pumps); i++) {
			if (this->pumps[i] != nullptr) {
				switch (int(this->pumps[i]->get_direction_degrees())) {
				case -90: {
					lbl_dx = x0 - gridsize; lbl_dy = y0 - gridsize; lbl_align = GraphletAlignment::RT;
					cpt_dx = x0 + gridsize; cpt_dy = y0 - gridsize; cpt_align = GraphletAlignment::LT;
				} break;
				case 90: {
					lbl_dx = x0 + gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::LB;
					cpt_dx = x0; cpt_dy = y0 + gridsize + gridsize; cpt_align = GraphletAlignment::CT;
				} break;
				case 180: {
					lbl_dx = x0 - gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::RB;
					cpt_dx = x0 + gridsize; cpt_dy = y0 + gridsize; cpt_align = GraphletAlignment::LB;
				} break;
				default: {
					lbl_dx = x0 + gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::LB;
					cpt_dx = x0 - gridsize; cpt_dy = y0 + gridsize; cpt_align = GraphletAlignment::RB;
				} break;
				}

				this->place_id_element(this->pumps[i], x0, y0, GraphletAlignment::CC);
				this->place_id_element(this->plabels[i], lbl_dx, lbl_dy, lbl_align);
				this->place_id_element(this->pcaptions[i], cpt_dx, cpt_dy, cpt_align);
			}
		}

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->valves); i++) {
			if (this->valves[i] != nullptr) {
				if (this->valves[i]->get_direction_degrees() == 0.0) {
					if (this->valves[i]->get_id<HPS>() == HPS::SQ2) {
						lbl_dx = x0 - adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::RC;
					} else {
						lbl_dx = x0 + adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::LC;
					}
				} else {
					lbl_dx = x0; lbl_dy = y0 - adjust_offset; lbl_align = GraphletAlignment::CB;
				}

				this->place_id_element(this->valves[i], x0, y0, GraphletAlignment::CC);
				this->place_id_element(this->vlabels[i], lbl_dx, lbl_dy, lbl_align);
			}
		}
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime) override {
		device->read_input_registers(0, 16);
	}

	void on_input_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {
		float Mpa = 0.0F;
		
		//this->workbench->enter_critical_section();
		
		//for (size_t idx = 1; idx < GRAPHLETS_LENGTH(this->gauges); idx++) {
		//	float mpa = float(register_values[idx]) * 0.1F;

		//	Mpa = Mpa + mpa;
		//	this->gauges[idx]->set_scale(mpa);
		//}

		//this->gauges[0]->set_scale(Mpa);

		//this->workbench->leave_critical_section();
	}

	void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Error, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	}

private:
	template<typename T>
	void load_graphlets(T* gs[], Labellet* ls[], HPS ids[], float radius, double degrees, size_t i0, size_t c, Labellet* cs[] = nullptr) {
		size_t in = i0 + c;
		
		for (size_t idx = i0; idx < in; idx++) {
			this->load_graphlet(gs, ls, idx, radius, degrees, ids[idx]);
		}

		if (cs != nullptr) {
			for (size_t idx = i0; idx < in; idx++) {
				cs[idx] = this->make_caption(ids[idx], Colours::Silver);
			}
		}
	}

	template<typename T>
	void load_graphlet(T* gs[], Labellet* ls[], size_t idx, float radius, double degrees, HPS id) {
		gs[idx] = new T(radius, degrees);
		ls[idx] = this->make_label(speak(id.ToString()), id, Colours::Silver);

		gs[idx]->set_id<HPS>(id);
		this->workbench->insert(gs[idx]);
	}

	Labellet* make_label(Platform::String^ caption, HPS id, CanvasSolidColorBrush^ color) {
		Labellet* label = new Labellet(caption);

		label->set_id(id);
		label->set_color(color);
		
		return this->workbench->insert_one(label);
	}

	Labellet* make_caption(HPS id, CanvasSolidColorBrush^ color) {
		return make_label(speak("HPS_" + id.ToString()), id, color);
	}

	void place_id_element(IGraphlet* g, float dx, float dy, GraphletAlignment scp) {
		float x, y;

		this->stations[0]->fill_anchor_location(g->get_id<HPS>(), &x, &y);
		this->workbench->move_to(g, x + dx, y + dy, scp);
	}

// never deletes these graphlets mannually
private:
	Labellet* captions[2];
	Tracklet<HPS>* stations[2];
	Pumplet* pumps[sizeof(pump_ids) / sizeof(HPS)];
	Labellet* plabels[sizeof(pump_ids) / sizeof(HPS)];
	Labellet* pcaptions[sizeof(pump_ids) / sizeof(HPS)];
	Valvelet* valves[sizeof(valve_ids) / sizeof(HPS)];
	Labellet* vlabels[sizeof(valve_ids) / sizeof(HPS)];

private:
	HPSingle* workbench;
};

HPSingle::HPSingle(Platform::String^ plc) : Planet(":hps:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HPS");
	HPSConsole* console = new HPSConsole(this);

	this->console = console; 
	this->device = new ModbusClient(alarm, plc, this->console);
	this->gridsize = statusbar_height();
}

HPSingle::~HPSingle() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}
}

void HPSingle::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HPSMode::View);
			console->load_pump_station(width, height, this->gridsize);
			console->load_pump_elements(width, height, this->gridsize);

			this->change_mode(HPSMode::WindowUI);
			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			PageDecorator* page = new PageDecorator(Colours::GrayText);
#ifdef _DEBUG
			IPlanetDecorator* decorators[] = { page, new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset) };

			this->set_decorator(new CompositeDecorator(decorators));
#else
			this->set_decorator(page);
#endif

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void HPSingle::reflow(float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HPSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAlignment::LB);

		this->change_mode(HPSMode::View);
		console->reflow_pump_station(width, height, this->gridsize, vinset);
		console->reflow_pump_elements(width, height, this->gridsize, vinset);
	}
}

void HPSingle::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(g);
}
