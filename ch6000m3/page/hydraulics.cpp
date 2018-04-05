#include "page/hydraulics.hpp"
#include "configuration.hpp"
#include "console.idl"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.idl"

#include "graphlet/gaugelet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/pumplet.hpp"
#include "graphlet/valvelet.hpp"

#include "decorator/page.hpp"
#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HSMode { WindowUI = 0, View };

// WARNING: order matters
private enum class HS : unsigned int {
	// for pumps
	A, B, G, H,
	F, C, D, E,
	Y, L, II, K,
	J, I,
	// for valves
	SQ1, SQ2, SQy, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk2, SQk1,
	SQf, SQc, SQd, SQe,
	// otheres
	F001, Port, Starboard, MasterTank, Heater, Visor, OilTank,
	_,
	// anchors used for default jumping back
	a, b, c, d, e, f, g, h, i, j, y, l, ii, k
};

private class Hydraulics final : public WarGrey::SCADA::ModbusConfirmation, public WarGrey::SCADA::Console<HydraulicSystem, HS> {
public:
	Hydraulics(HydraulicSystem* master) : Console(master, "HS") {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HS>* pTurtle = new Turtle<HS>(gridsize, true, HS::SQ1);

		pTurtle->move_down()->turn_down_right()->move_right(12, HS::l)->turn_right_down()->move_down(6);
		
		pTurtle->move_down(3, HS::f)->move_right(6, HS::SQf)->move_right(8, HS::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::c)->move_right(6, HS::SQc)->move_right(8, HS::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::d)->move_right(6, HS::SQd)->move_right(8, HS::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::e)->move_right(6, HS::SQe)->move_right(8, HS::E)->move_right(6);
		
		pTurtle->move_up(12, HS::Starboard)->move_up(20)->turn_up_left()->move_left(35);
		pTurtle->turn_left_down()->move_down(4, HS::MasterTank)->jump_left(4);
		pTurtle->move_up(4)->turn_up_left()->move_left(31)->turn_left_down()->move_down(20);

		pTurtle->move_down(3, HS::a)->move_right(6, HS::A)->move_right(8, HS::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::b)->move_right(6, HS::B)->move_right(8, HS::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::g)->move_right(6, HS::G)->move_right(8, HS::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::h)->move_right(6, HS::H)->move_right(8, HS::SQh)->move_right(6);

		pTurtle->move_up(12, HS::Port)->move_up(6)->turn_up_right()->move_right(12)->turn_right_up()->move_up(1, HS::SQ2);

		pTurtle->jump_back(HS::l);
		pTurtle->jump_left(5, HS::y)->turn_right_up()->move_up(4, HS::SQy)->move_up(4, HS::Y)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::l)->turn_right_up()->move_up(8, HS::L)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::ii)->turn_right_up()->move_up(8, HS::II)->move_up(4)->jump_back();
		pTurtle->move_right(3, HS::SQk2)->move_right(3, HS::k)->move_up(9, HS::K)->move_up(3)->turn_up_left();
		pTurtle->move_left(20)->turn_left_down()->move_down(1.5F, HS::F001)->move_down(1.5F);

		pTurtle->jump_back(HS::k)->move_right(3, HS::SQk1)->move_right(2, HS::OilTank);

		pTurtle->jump_back(HS::SQ1)->jump_down(11.8F)->move_down(2, HS::SQi)->move_down(3, HS::I)->move_down(3);
		pTurtle->jump_back(HS::SQ2)->jump_down(11.8F)->move_down(2, HS::SQj)->move_down(3, HS::J)->move_down(3);
		this->stations[0] = new Tracklet<HS>(pTurtle, 1.5F, Colours::DimGray);

		pTurtle->wipe();
		pTurtle->jump_back(HS::SQ1)->move_up(2);
		pTurtle->jump_back(HS::SQ2)->move_up(2);
		this->stations[1] = new Tracklet<HS>(pTurtle, 1.5F, Colours::Yellow); 

		this->load_label(this->captions, HS::MasterTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);

		this->heater = new LevelGaugelet(gridsize * 14.0F, gridsize * 6.0F, 1.5F);
		//this->visor = new LevelGaugelet(gridsize * 12.0F, gridsize * 4.0F, 0.8F);

		this->master->insert_all(this->stations, true);
		this->master->insert(this->heater);
		//this->master->insert(this->visor);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_graphlets(this->pumps, this->plabels, HS::A, HS::H, gridsize, 180.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::F, HS::E, gridsize, 0.000, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::Y, HS::K, gridsize, -90.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::J, HS::I, gridsize, 90.00, this->pcaptions);
		}

		{ // load valves
			this->load_graphlets(this->valves, this->vlabels, HS::SQ1, HS::SQj, gridsize, 0.000);
			this->load_graphlets(this->valves, this->vlabels, HS::SQa, HS::SQk1, gridsize, -90.0);
			this->load_graphlets(this->valves, this->vlabels, HS::SQf, HS::SQe, gridsize, 90.00);
		}
	}

	void load_indicators(float width, float height, float gridsize) {
		this->master_indicators[0] = this->make_indicator("hpc_master_T", Colours::Green);
		this->master_indicators[1] = this->make_indicator("hpc_master_low", Colours::Green);
		this->master_indicators[2] = this->make_indicator("hpc_master_low2", Colours::Green);
		this->master_indicators[3] = this->make_indicator("hpc_master_high", Colours::Green);
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float sw, sh, sx, sy, s1_x, s1_y;

		this->stations[0]->fill_extent(0.0F, 0.0F, &sw, &sh);
		this->stations[0]->fill_anchor_location(HS::SQ1, &s1_x, &s1_y);

		sx = (width - sw) * 0.5F;
		sy = (height - sh) * 0.5F - vinset * 0.5F;
		this->master->move_to(this->stations[0], sx, sy);
		
		for (auto lt = this->captions.begin(); lt != this->captions.end(); lt++) {
			if (lt->second->id == HS::MasterTank) {
				this->master->move_to(lt->second, sx + gridsize * 6.0F, sy + gridsize * 3.0F);
			} else {
				this->stations[0]->map_credit_graphlet(lt->second, -gridsize, 0.0F, GraphletAlignment::RB);
			}
		}

		this->master->move_to(this->stations[1], sx + s1_x, sy + s1_y, GraphletAlignment::RB);
		this->master->move_to(this->heater, sx + sw * 0.5F, sy + s1_y - gridsize * 1.5F, GraphletAlignment::CB);
		//this->master->move_to(this->visor, sx + sw * 0.5F, sy + s1_y + gridsize * 12.0F, GraphletAlignment::CB);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy;
		float x0 = 0.0F;
		float y0 = 0.0F;
		float adjust_offset = gridsize * 0.8F;
		GraphletAlignment lbl_align, cpt_align;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (int(it->second->get_direction_degrees())) {
			case -90: { // for Y, L, II, K
				lbl_dx = x0 - gridsize; lbl_dy = y0 - gridsize; lbl_align = GraphletAlignment::RT;
				cpt_dx = x0 + vinset * 0.25F; cpt_dy = y0 - gridsize; cpt_align = GraphletAlignment::LB;
			} break;
			case 90: {  // for J, I
				lbl_dx = x0 + gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::LB;
				cpt_dx = x0; cpt_dy = y0 + gridsize * 3.0F; cpt_align = GraphletAlignment::CT;
			} break;
			case 180: { // for A, B, G, H
				lbl_dx = x0 - gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::RB;
				cpt_dx = x0 + gridsize; cpt_dy = y0 + gridsize; cpt_align = GraphletAlignment::LB;
			} break;
			default: {  // for F, C, D, E
				lbl_dx = x0 + gridsize; lbl_dy = y0 + gridsize; lbl_align = GraphletAlignment::LB;
				cpt_dx = x0 - gridsize; cpt_dy = y0 + gridsize; cpt_align = GraphletAlignment::RB;
			}
			}

			this->stations[0]->map_credit_graphlet(it->second, x0, y0, GraphletAlignment::CC);
			this->stations[0]->map_credit_graphlet(this->plabels[it->first], lbl_dx, lbl_dy, lbl_align);
			this->stations[0]->map_credit_graphlet(this->pcaptions[it->first], cpt_dx, cpt_dy, cpt_align);
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 0.0) {
				if (it->second->id == HS::SQ2) {
					lbl_dx = x0 - adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::RC;
				} else {
					lbl_dx = x0 + adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::LC;
				}
			} else {
				lbl_dx = x0; lbl_dy = y0 - adjust_offset; lbl_align = GraphletAlignment::CB;
			}

			this->stations[0]->map_credit_graphlet(it->second, x0, y0, GraphletAlignment::CC);
			this->stations[0]->map_credit_graphlet(this->vlabels[it->first], lbl_dx, lbl_dy, lbl_align);
		}
	}

	void reflow_indicators(float width, float height, float gridsize, float vinset) {
		float x0, y0, indicator_x, indicator_y, lineheight;

		this->master->fill_graphlet_location(this->stations[0], &x0, &y0);
		this->master_indicators[0]->fill_extent(0.0F, 0.0F, nullptr, &lineheight);

		indicator_x = x0 + gridsize * 5.0F;
		indicator_y = y0 + gridsize * 5.0F;
		lineheight *= 1.2F;
		for (unsigned int i = 0; i < GRAPHLETS_LENGTH(this->master_indicators); i++) {
			this->master->move_to(this->master_indicators[i], indicator_x, indicator_y + lineheight * float(i));
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

// never deletes these graphlets mannually
private:
	Tracklet<HS>* stations[2];
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Pumplet, HS>*> pumps;
	std::map<HS, Credit<Labellet, HS>*> plabels;
	std::map<HS, Credit<Labellet, HS>*> pcaptions;
	std::map<HS, Credit<Valvelet, HS>*> valves;
	std::map<HS, Credit<Labellet, HS>*> vlabels;
	Credit<Booleanlet, HS>* master_indicators[4];
	LevelGaugelet* heater;
	LevelGaugelet* visor;

private:
	CanvasTextFormat^ caption_font;
};

HydraulicSystem::HydraulicSystem(Platform::String^ plc) : Planet(":hs:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HS");
	Hydraulics* console = new Hydraulics(this);

	this->console = console; 
	this->device = new ModbusClient(alarm, plc, this->console);
	this->gridsize = statusbar_height();
}

HydraulicSystem::~HydraulicSystem() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}
}

void HydraulicSystem::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<Hydraulics*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HSMode::View);
			console->load_pump_station(width, height, this->gridsize);
			console->load_devices(width, height, this->gridsize);
			console->load_indicators(width, height, this->gridsize);

			this->change_mode(HSMode::WindowUI);
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

void HydraulicSystem::reflow(float width, float height) {
	auto console = dynamic_cast<Hydraulics*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAlignment::LB);

		this->change_mode(HSMode::View);
		console->reflow_pump_station(width, height, this->gridsize, vinset);
		console->reflow_devices(width, height, this->gridsize, vinset);
		console->reflow_indicators(width, height, this->gridsize, vinset);
	}
}

void HydraulicSystem::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	if (dynamic_cast<Tracklet<HS>*>(g) == nullptr) {
		this->set_selected(g);
	}
	// this->set_caret_owner(g);
}
