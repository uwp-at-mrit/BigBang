#include "page/hp_single.hpp"
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

private enum class HPS {
	A, B, C, D, E, F, G, H, I, J, K, Y, F001, SQ1, SQ2, SQ3,
	SQa, SQb, SQc, SQd, SQe, SQf, SQg, SQh, SQi, SQj, SQk, SQy,
	Port, Starboard,
	_,
	a, b, c, d, e, f, g, h, k
};

private class HPSConsole : public WarGrey::SCADA::ModbusConfirmation {
public:
	HPSConsole(HPSingle* master) : workbench(master) {}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HPS>* pTurtle = new Turtle<HPS>(gridsize, true, HPS::SQ1);

		pTurtle->move_down(4)->turn_down_right()->move_right(10)->turn_right_down();
		pTurtle->move_down(4, HPS::f)->move_right(4, HPS::SQf)->move_right(10, HPS::F)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::c)->move_right(4, HPS::SQc)->move_right(10, HPS::C)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::d)->move_right(4, HPS::SQd)->move_right(10, HPS::D)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::e)->move_right(4, HPS::SQe)->move_right(10, HPS::E)->move_right(4);
		pTurtle->move_up(16, HPS::Starboard)->move_up(14)->turn_up_left()->move_left(32);
		pTurtle->turn_left_down()->move_down(1.5F, HPS::F001)->move_down(1.5F)->jump_up(3)->turn_up_left()->move_left(26);
		pTurtle->turn_left_down()->move_down(14, HPS::Port)->move_down(3);
		pTurtle->move_down(4, HPS::a)->move_right(4, HPS::A)->move_right(10, HPS::SQa)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::b)->move_right(4, HPS::B)->move_right(10, HPS::SQb)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::g)->move_right(4, HPS::G)->move_right(10, HPS::SQg)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPS::h)->move_right(4, HPS::H)->move_right(10, HPS::SQh)->move_right(4)->move_up(16);
		pTurtle->turn_up_right()->move_right(8)->turn_right_up()->move_up(1, HPS::SQ2);
		pTurtle->jump_right(8, HPS::SQ3)->move_down()->turn_down_right()->move_right(8, HPS::k);
		pTurtle->move_right(4, HPS::SQk)->move_right(4)->turn_right_up()->move_up(8, HPS::K)->move_up(5)->jump_back();
		pTurtle->move_up(5, HPS::SQy)->move_up(4, HPS::Y)->move_up(5);

		this->stations[0] = new Tracklet<HPS>(pTurtle, 1.5F, Colours::Goldenrod);

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->stations); i++) {
			if (this->stations[i] != nullptr) {
				this->workbench->insert(this->stations[i]);
			}
		}
	}

	void load_pump_elements(float width, float height, float gridsize) {
		{ // load pumps
			HPS pids[] = {
				HPS::A, HPS::B, HPS::G, HPS::H,
				HPS::F, HPS::C, HPS::D, HPS::E,
				HPS::Y, HPS::K
			};

			this->load_devices(this->pumps, this->plabels, pids, gridsize, 180.0, 0, 4, this->pcaptions);
			this->load_devices(this->pumps, this->plabels, pids, gridsize, 0.000, 4, 4, this->pcaptions);
			this->load_devices(this->pumps, this->plabels, pids, gridsize, -90.0, 8, 2, this->pcaptions);
		}

		{ // load valves
			float adjust_gridsize = gridsize * 1.2F;
			HPS vids[] = {
				HPS::SQ1, HPS::SQ2, HPS::SQ3, HPS::SQy,
				HPS::SQa, HPS::SQb, HPS::SQg, HPS::SQh, HPS::SQk,
				HPS::SQf, HPS::SQc, HPS::SQd, HPS::SQe,
			};

			this->load_devices(this->valves, this->vlabels, vids, adjust_gridsize, 0.000, 0, 4);
			this->load_devices(this->valves, this->vlabels, vids, adjust_gridsize, -90.0, 4, 5);
			this->load_devices(this->valves, this->vlabels, vids, adjust_gridsize, 90.00, 9, 4);
		}
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float station_width, station_height;

		this->stations[0]->fill_extent(0.0F, 0.0F, &station_width, &station_height);
		this->workbench->move_to(this->stations[0], (width - station_width) * 0.5F, (height - station_height) * 0.5F);
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
					lbl_dx = x0 - adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::RC;
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
	void load_devices(T* gs[], Labellet* ls[], HPS ids[], float radius, double degrees, size_t i0, size_t c, Labellet* cs[] = nullptr) {
		size_t in = i0 + c;
		
		for (size_t idx = i0; idx < in; idx++) {
			this->load_device(gs, ls, idx, radius, degrees, ids[idx]);
		}

		if (cs != nullptr) {
			for (size_t idx = i0; idx < in; idx++) {
				cs[idx] = this->make_label(speak("HPS_" + ids[idx].ToString()), ids[idx]);
			}
		}
	}

	template<typename T>
	void load_device(T* gs[], Labellet* ls[], size_t idx, float radius, double degrees, HPS id) {
		gs[idx] = new T(radius, degrees);
		ls[idx] = this->make_label(speak(id.ToString()), id);

		gs[idx]->id = static_cast<long>(id);
		this->workbench->insert(gs[idx]);
	}

	Labellet* make_label(Platform::String^ caption, HPS id = HPS::_) {
		Labellet* label = new Labellet(caption);

		label->id = static_cast<long>(id);
		this->workbench->insert(label);

		return label;
	}

	void place_id_element(IGraphlet* g, float dx, float dy, GraphletAlignment scp) {
		float x, y;

		this->stations[0]->fill_anchor_location(static_cast<HPS>(g->id), &x, &y);
		this->workbench->move_to(g, x + dx, y + dy, scp);
	}

// never deletes these graphlets mannually
private:
	Labellet* labels[2];
	Tracklet<HPS>* stations[2];
	Pumplet* pumps[12];
	Labellet* plabels[12];
	Labellet* pcaptions[12];
	Valvelet* valves[13];
	Labellet* vlabels[13];

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
			GridDecorator* grid = new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset);
			IPlanetDecorator* decorators[] = { new PageDecorator(Colours::GrayText), grid };

			this->set_decorator(MAKE_COMPOSE_DECORATOR(decorators));

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
