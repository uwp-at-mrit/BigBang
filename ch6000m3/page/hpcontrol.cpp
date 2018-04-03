#include "page/hpcontrol.hpp"
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

private enum HPCMode { WindowUI = 0, View };

// WARNING: order matters
private enum class HPC : unsigned int {
	// for pumps
	A, B, G, H,
	F, C, D, E,
	Y, K,
	J, I,
	// for valves
	SQ1, SQ2, SQ3, SQy, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk,
	SQf, SQc, SQd, SQe,
	// otheres
	F001, Port, Starboard, MasterTank,
	_,
	// anchors used for jumping back
	a, b, c, d, e, f, g, h, i, j, k
};

private class HPSConsole final : public WarGrey::SCADA::ModbusConfirmation, public WarGrey::SCADA::Console<HPControl, HPC> {
public:
	HPSConsole(HPControl* master) : Console(master, "HPC") {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HPC>* pTurtle = new Turtle<HPC>(gridsize, true, HPC::SQ1);

		pTurtle->move_down(4)->turn_down_right()->move_right(10)->turn_right_down();
		pTurtle->move_down(5, HPC::f)->move_right(4, HPC::SQf)->move_right(10, HPC::F)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::c)->move_right(4, HPC::SQc)->move_right(10, HPC::C)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::d)->move_right(4, HPC::SQd)->move_right(10, HPC::D)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::e)->move_right(4, HPC::SQe)->move_right(10, HPC::E)->move_right(4)->move_up(12, HPC::Starboard);
		pTurtle->move_up(19)->turn_up_left()->move_left(32);
		pTurtle->turn_left_down(HPC::f)->move_down(1.5F, HPC::F001)->move_down(1.5F, HPC::MasterTank)->move_down(2)->jump_back();
		pTurtle->turn_up_left()->move_left(26)->turn_left_down()->move_down(17);
		pTurtle->move_down(5, HPC::a)->move_right(4, HPC::A)->move_right(10, HPC::SQa)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::b)->move_right(4, HPC::B)->move_right(10, HPC::SQb)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::g)->move_right(4, HPC::G)->move_right(10, HPC::SQg)->move_right(4)->jump_back();
		pTurtle->move_down(3, HPC::h)->move_right(4, HPC::H)->move_right(10, HPC::SQh)->move_right(4)->move_up(12, HPC::Port);
		pTurtle->move_up(5)->turn_up_right()->move_right(8)->turn_right_up();
		pTurtle->move_up(1, HPC::SQ2)->jump_right(8, HPC::SQ3)->move_down()->turn_down_right();
		pTurtle->move_right(8, HPC::k)->move_right(4, HPC::SQk)->move_right(4);
		pTurtle->turn_right_up()->move_up(8, HPC::K)->move_up(5)->jump_back();
		pTurtle->move_up(5, HPC::SQy)->move_up(4, HPC::Y)->move_up(5);

		pTurtle->jump_back(HPC::SQ2)->jump_down(12.8F, HPC::j)->move_down(2, HPC::SQj)->move_down(3, HPC::J)->move_down(2);
		pTurtle->jump_back()->jump_right(8, HPC::i)->move_down(2, HPC::SQi)->move_down(3, HPC::I)->move_down(2);
		this->stations[0] = new Tracklet<HPC>(pTurtle, 1.5F, Colours::DimGray);

		pTurtle->wipe();
		pTurtle->jump_back(HPC::SQ1)->move_up(2);
		pTurtle->jump_back(HPC::SQ2)->move_up(2);
		pTurtle->jump_back(HPC::SQ3)->move_up(2);
		this->stations[1] = new Tracklet<HPC>(pTurtle, 1.5F, Colours::Yellow); 

		this->load_label(this->captions, HPC::MasterTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HPC::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HPC::Starboard, Colours::DarkKhaki, this->caption_font);

		this->oiltanks[0] = new LiquidGaugelet(1.5F, 80.0F);
		//this->oiltanks[1] = new LiquidGaugelet(0.7F, 80.0F);

		this->master->insert_all(this->stations, true);
		this->master->insert(this->oiltanks[0]);
		//this->master->insert(this->oiltanks[1]);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_graphlets(this->pumps, this->plabels, HPC::A, HPC::H, gridsize, 180.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HPC::F, HPC::E, gridsize, 0.000, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HPC::Y, HPC::K, gridsize, -90.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HPC::J, HPC::I, gridsize, 90.00, this->pcaptions);
		}

		{ // load valves
			float adjust_gridsize = gridsize * 1.2F;
			
			this->load_graphlets(this->valves, this->vlabels, HPC::SQ1, HPC::SQj, adjust_gridsize, 0.000);
			this->load_graphlets(this->valves, this->vlabels, HPC::SQa, HPC::SQk, adjust_gridsize, -90.0);
			this->load_graphlets(this->valves, this->vlabels, HPC::SQf, HPC::SQe, adjust_gridsize, 90.00);
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
		this->stations[0]->fill_anchor_location(HPC::SQ1, &s1_x, &s1_y);

		sx = (width - sw) * 0.5F;
		sy = (height - sh) * 0.5F;
		this->master->move_to(this->stations[0], sx, sy);
		
		for (auto lt = this->captions.begin(); lt != this->captions.end(); lt++) {
			if (lt->second->id == HPC::MasterTank) {
				this->master->move_to(lt->second, sx + gridsize * 6.0F, sy + gridsize * 3.0F);
			} else {
				this->place_id_element(this->stations[0], lt->second, sx - gridsize, sy, GraphletAlignment::RB);
			}
		}

		this->master->move_to(this->stations[1], sx + s1_x, sy + s1_y, GraphletAlignment::CB);
		this->master->move_to(this->oiltanks[0], sx + s1_x, sy + s1_y - gridsize * 1.5F, GraphletAlignment::CB);
		//this->master->move_to(this->oiltanks[1], sx + s1_x, sy + s1_y + gridsize * 8.0F, GraphletAlignment::CT);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		float x0, y0, lbl_dx, lbl_dy, cpt_dx, cpt_dy;
		float adjust_offset = gridsize * 0.8F;
		GraphletAlignment lbl_align, cpt_align;

		this->master->fill_graphlet_location(this->stations[0], &x0, &y0);

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (int(it->second->get_direction_degrees())) {
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
			}
			}

			this->place_id_element(this->stations[0], it->second, x0, y0, GraphletAlignment::CC);
			this->place_id_element(this->stations[0], this->plabels[it->first], lbl_dx, lbl_dy, lbl_align);
			this->place_id_element(this->stations[0], this->pcaptions[it->first], cpt_dx, cpt_dy, cpt_align);
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 0.0) {
				if (it->second->id == HPC::SQ2) {
					lbl_dx = x0 - adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::RC;
				} else {
					lbl_dx = x0 + adjust_offset; lbl_dy = y0; lbl_align = GraphletAlignment::LC;
				}
			} else {
				lbl_dx = x0; lbl_dy = y0 - adjust_offset; lbl_align = GraphletAlignment::CB;
			}

			this->place_id_element(this->stations[0], it->second, x0, y0, GraphletAlignment::CC);
			this->place_id_element(this->stations[0], this->vlabels[it->first], lbl_dx, lbl_dy, lbl_align);
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
	Tracklet<HPC>* stations[2];
	std::map<HPC, Credit<Labellet, HPC>*> captions;
	std::map<HPC, Credit<Pumplet, HPC>*> pumps;
	std::map<HPC, Credit<Labellet, HPC>*> plabels;
	std::map<HPC, Credit<Labellet, HPC>*> pcaptions;
	std::map<HPC, Credit<Valvelet, HPC>*> valves;
	std::map<HPC, Credit<Labellet, HPC>*> vlabels;
	Credit<Booleanlet, HPC>* master_indicators[4];
	IGaugelet* oiltanks[2];

private:
	CanvasTextFormat^ caption_font;
};

HPControl::HPControl(Platform::String^ plc) : Planet(":hpc:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HPC");
	HPSConsole* console = new HPSConsole(this);

	this->console = console; 
	this->device = new ModbusClient(alarm, plc, this->console);
	this->gridsize = statusbar_height();
}

HPControl::~HPControl() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}
}

void HPControl::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HPCMode::View);
			console->load_pump_station(width, height, this->gridsize);
			console->load_devices(width, height, this->gridsize);
			console->load_indicators(width, height, this->gridsize);

			this->change_mode(HPCMode::WindowUI);
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

void HPControl::reflow(float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HPCMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAlignment::LB);

		this->change_mode(HPCMode::View);
		console->reflow_pump_station(width, height, this->gridsize, vinset);
		console->reflow_devices(width, height, this->gridsize, vinset);
		console->reflow_indicators(width, height, this->gridsize, vinset);
	}
}

void HPControl::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	if (dynamic_cast<Tracklet<HPC>*>(g) == nullptr) {
		this->set_selected(g);
	}
	// this->set_caret_owner(g);
}
