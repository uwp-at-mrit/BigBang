﻿#include <map>

#include "page/hydraulics.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/booleanlet.hpp"
#include "graphlet/shapelet.hpp"

#include "graphlet/symbol/pumplet.hpp"
#include "graphlet/symbol/valvelet.hpp"
#include "graphlet/device/tanklet.hpp"
#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/thermometerlet.hpp"

#include "decorator/page.hpp"
#ifdef _DEBUG
#include "decorator/grid.hpp"
#endif

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HSMode { WindowUI = 0, Dashboard };

private enum class HSOperation { Start, Stop, Reset, _ };
private enum class HSMTStatus { Empty, UltraLow, Low, Normal, High, Full, _ };
private enum class HSVTStatus { Empty, UltraLow, Low, Normal, Full, _ };

// WARNING: order matters
private enum class HS : unsigned int {
	// Pumps
	A, B, G, H,
	F, C, D, E,
	J, I,
	Y, L, M, K,
	// Valves
	SQ1, SQ2, SQy, SQl, SQm, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk2, SQk1,
	SQf, SQc, SQd, SQe,
	// Key Labels
	Port, Starboard, Master, Visor, Storage, Pressure,
	// Indicators
	F001Blocked, F002Blocked,
	TS1, TS2, TS11, TS12,
	FilterBlocked,
	_,
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, i, j, y, l, m, k,

	// anchors used for unnamed corners
};

private class Hydraulics final : public PLCConfirmation, public IMenuCommand<HSOperation, IMRMaster*> {
public:
	Hydraulics(HydraulicsPage* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei", large_font_size);
		this->number_font = make_bold_text_format("Cambria Math", 20.0F);
		this->unit_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->temperatures[HS::Master]->set_value(RealData(AI_DB203, 18U));
		this->temperatures[HS::Visor]->set_value(RealData(AI_DB203, 19U));

		{ // pump pressures
			static std::map<HS, bool> need_lock_position;
			HS bar_seq[] = { HS::C, HS::F, HS::D, HS::E, HS::A, HS::B, HS::G, HS::H, HS::I, HS::J };

			if (need_lock_position.size() == 0) {
				need_lock_position[HS::F] = true;
				need_lock_position[HS::C] = true;
				need_lock_position[HS::D] = true;
				need_lock_position[HS::E] = true;
				need_lock_position[HS::J] = true;
			}

			for (size_t i = 0; i < sizeof(bar_seq) / sizeof(HS); i++) {
				HS id = bar_seq[i];
				
				if (need_lock_position[id]) {
					this->pressures[id]->set_value(RealData(AI_DB203, 8 + i), GraphletAnchor::RB);
				} else {
					this->pressures[id]->set_value(RealData(AI_DB203, 8 + i));
				}
			}
		}

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		{ // pump states
			HS pump_seq[] = { HS::A, HS::B, HS::C, HS::D, HS::E, HS::G, HS::G, HS::H, HS::Y, HS::K, HS::L, HS::M, HS::I, HS::J };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(HS); i++) {
				Pumplet* target = this->pumps[pump_seq[i]];

				switch (DI_db205_X[i]) {
				case 0b00000001: target->set_status(PumpStatus::Starting); break;
				case 0b00000010: target->set_status(PumpStatus::Stopping); break;
				case 0b00000100: target->set_status(PumpStatus::Unstartable); break;
				case 0b00001000: target->set_status(PumpStatus::Unstoppable); break;
				case 0b00010000: target->set_status(PumpStatus::Running); break;
				case 0b00100000: target->set_status(PumpStatus::Stopped); break;
				case 0b01000000: target->set_status(PumpStatus::Remote); break;
				case 0b10000000: target->set_status(PumpStatus::Ready); break;
				}
			}
		}

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(HSOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto pump = dynamic_cast<Credit<Pumplet, HS>*>(target);

		if (pump != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				pump->id.ToString()->Data());
		}
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HS>* pTurtle = new Turtle<HS>(gridsize, true, HS::Master);

		pTurtle->move_right(2)->move_down(5.5F, HS::SQ1);
		pTurtle->move_down()->turn_down_right()->move_right(13, HS::l)->turn_right_down()->move_down(5);
		
		pTurtle->move_down(3, HS::f)->move_right(6, HS::SQf)->move_right(8, HS::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::c)->move_right(6, HS::SQc)->move_right(8, HS::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::d)->move_right(6, HS::SQd)->move_right(8, HS::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::e)->move_right(6, HS::SQe)->move_right(8, HS::E)->move_right(6);
		
		pTurtle->move_up(11, HS::Starboard)->move_up(22)->turn_up_left()->move_left(35);
		pTurtle->turn_left_down()->move_down(2, HS::F001Blocked)->move_down(2);
		pTurtle->jump_up(4)->turn_up_left()->move_left(35)->turn_left_down()->move_down(21);

		pTurtle->move_down(3, HS::a)->move_right(6, HS::A)->move_right(8, HS::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::b)->move_right(6, HS::B)->move_right(8, HS::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::g)->move_right(6, HS::G)->move_right(8, HS::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::h)->move_right(6, HS::H)->move_right(8, HS::SQh)->move_right(6);

		pTurtle->move_up(12, HS::Port)->move_up(5)->turn_up_right()->move_right(13)->turn_right_up()->move_up(1, HS::SQ2);
		pTurtle->move_up(5.5F)->move_right(2 /* HS::Master */);

		pTurtle->jump_back(HS::l);
		pTurtle->jump_left(5, HS::y)->turn_right_up()->move_up(4, HS::SQy)->move_up(4, HS::Y)->move_up(6)->jump_back();
		pTurtle->move_right(5, HS::l)->turn_right_up()->move_up(4, HS::SQl)->move_up(4, HS::L)->move_up(6)->jump_back();
		pTurtle->move_right(5, HS::m)->turn_right_up()->move_up(4, HS::SQm)->move_up(4, HS::M)->move_up(6)->jump_back();
		pTurtle->move_right(3, HS::SQk2)->move_right(3, HS::k)->move_up(9, HS::K)->move_up(5)->turn_up_left();
		pTurtle->move_left(21)->turn_left_down()->move_down(HS::F002Blocked)->move_down(2);

		pTurtle->jump_back(HS::k)->move_right(3, HS::SQk1)->move_right(2.5F, HS::Storage);
		
		pTurtle->jump_back(HS::Master)->jump_down(14, HS::Visor);
		pTurtle->move_right(2)->move_down(5, HS::SQi)->move_down(3, HS::I)->move_down(3);
		pTurtle->jump_left(4)->move_up(3, HS::J)->move_up(3, HS::SQj)->move_up(5)->move_right(2 /* HS::Visor */);
		
		this->station = this->master->insert_one(new Tracklet<HS>(pTurtle, 1.5F, Colours::Gray));
		
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Storage, Colours::Silver);
	}

	void load_tanks(float width, float height, float gridsize) {
		float thickness = 3.0F;

		this->master_tank = this->make_tank(HSMTStatus::High, gridsize * 17.0F, gridsize * 8.0F, thickness);
		this->visor_tank = this->make_tank(HSVTStatus::Normal, gridsize * 15.0F, gridsize * 6.0F, thickness);

		this->load_thermometer(this->thermometers, this->temperatures, HS::Master, gridsize * 2.5F);
		this->load_thermometer(this->thermometers, this->temperatures, HS::Visor, gridsize * 2.5F);

		this->storage_tank = this->master->insert_one(new FuelTanklet(gridsize * 2.5F, 0.0F, thickness, Colours::WhiteSmoke));
		
		this->load_boolean_indicators(HS::F001Blocked, HS::FilterBlocked, gridsize, this->states, this->islabels);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::A, HS::H, gridsize, 180.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::F, HS::E, gridsize, 0.000);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::Y, HS::K, gridsize, -90.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::J, HS::I, gridsize, 90.00);

			this->load_dimensions(this->pressures, HS::A, HS::I, "bar");

			this->station_pressure = new Dimensionlet("bar", _speak(HS::Pressure), "", this->number_font, this->unit_font);
			this->master->insert(this->station_pressure);
		}

		{ // load valves
			this->load_devices(this->valves, this->vlabels, HS::SQ1, HS::SQj, gridsize, 0.000);
			this->load_devices(this->valves, this->vlabels, HS::SQa, HS::SQk1, gridsize, -90.0);
			this->load_devices(this->valves, this->vlabels, HS::SQf, HS::SQe, gridsize, 90.00);
		}
	}

public:
	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float sq1_y;

		this->master->move_to(this->station, cx, cy, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->storage_tank, HS::Storage, GraphletAnchor::LC);
		this->station->fill_anchor_location(HS::SQ1, nullptr, &sq1_y, true);
		this->station->map_graphlet_at_anchor(this->master_tank, HS::Master, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->visor_tank, HS::Visor, GraphletAnchor::CC);
		this->master->move_to(this->station_pressure, this->station, GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->thermometers[HS::Master], this->master_tank, 0.25F, 0.5F, GraphletAnchor::CC);
		this->master->move_to(this->thermometers[HS::Visor], this->visor_tank, 0.25F, 0.5F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[HS::Port], GraphletAnchor::CB, -gridsize * 10.0F);
		this->station->map_credit_graphlet(this->captions[HS::Starboard], GraphletAnchor::CB, -gridsize * 10.0F);
		this->master->move_to(this->captions[HS::Storage], this->storage_tank, GraphletAnchor::CB, GraphletAnchor::CT);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		GraphletAnchor lbl_a, cpt_a, bar_a;
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy, bar_dx, bar_dy;
		float valve_adjust_gridsize = gridsize * 0.618F;
		float text_hspace = vinset * 0.125F;
		float x0 = 0.0F;
		float y0 = 0.0F;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (it->second->id) {
			case HS::A: case HS::B: case HS::G: case HS::H: {
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
				cpt_dx = x0 + gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::LT;
				bar_dx = x0 + gridsize; bar_dy = y0; bar_a = GraphletAnchor::LB;
			} break;
			case HS::F: case HS::C: case HS::D: case HS::E: {
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
				cpt_dx = x0 - gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::RT;
				bar_dx = x0 - gridsize; bar_dy = y0; bar_a = GraphletAnchor::RB;
			} break;
			case HS::Y: case HS::L: case HS::M: case HS::K: {
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RB;
				cpt_dx = x0 + text_hspace; cpt_dy = y0 - gridsize; cpt_a = GraphletAnchor::LB;
				bar_dx = x0; bar_dy = y0; bar_a = GraphletAnchor::CC; // these devices have no metrics
			} break;
			default: {
				cpt_dx = x0; cpt_dy = y0 + gridsize * 3.0F; cpt_a = GraphletAnchor::CT;
			
				if (it->second->id == HS::I) {
					lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
					bar_dx = x0 + text_hspace; bar_dy = y0 + gridsize; bar_a = GraphletAnchor::LT;
				} else {
					lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
					bar_dx = x0 - text_hspace; bar_dy = y0 + gridsize; bar_a = GraphletAnchor::RT;
				}
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->plabels[it->first], lbl_a, lbl_dx, lbl_dy);
			this->station->map_credit_graphlet(this->pcaptions[it->first], cpt_a, cpt_dx, cpt_dy);

			if (this->pressures.find(it->first) != this->pressures.end()) {
				this->station->map_credit_graphlet(this->pressures[it->first], bar_a, bar_dx, bar_dy);
			}
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 0.0) {
				switch (it->first) {
				case HS::SQ2: case HS::SQy: {
					lbl_dx = x0 - valve_adjust_gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RC;
				} break;
				default: {
					lbl_dx = x0 + valve_adjust_gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LC;
				}
				}
			} else {
				lbl_dx = x0; lbl_dy = y0 - valve_adjust_gridsize; lbl_a = GraphletAnchor::CB;
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->vlabels[it->first], lbl_a, lbl_dx, lbl_dy);
		}
	}

	void reflow_metrics(float width, float height, float gridsize, float vinset) {
		float vgapsize = gridsize * 0.125F;

		this->station->map_credit_graphlet(this->states[HS::F001Blocked], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->states[HS::F002Blocked], GraphletAnchor::CC);
		
		this->master->move_to(this->temperatures[HS::Master], this->thermometers[HS::Master], GraphletAnchor::RB, GraphletAnchor::LB, gridsize);
		this->master->move_to(this->states[HS::TS2], this->thermometers[HS::Master], GraphletAnchor::RC, GraphletAnchor::LB, gridsize);
		this->master->move_to(this->states[HS::TS1], this->states[HS::TS2], GraphletAnchor::LT, GraphletAnchor::LB, 0.0F, -vgapsize);

		this->master->move_to(this->temperatures[HS::Visor], this->thermometers[HS::Visor], GraphletAnchor::RB, GraphletAnchor::LB, gridsize);
		this->master->move_to(this->states[HS::FilterBlocked], this->thermometers[HS::Visor], GraphletAnchor::RC, GraphletAnchor::LT, gridsize);
		this->master->move_to(this->states[HS::TS12], this->states[HS::FilterBlocked], GraphletAnchor::LT, GraphletAnchor::LB, 0.0F, -vgapsize);
		this->master->move_to(this->states[HS::TS11], this->states[HS::TS12], GraphletAnchor::LT, GraphletAnchor::LB, 0.0F, -vgapsize);

		{ // reflow state labels
			float gapsize = vinset * 0.25F;

			for (auto lt = this->states.begin(); lt != this->states.end(); lt++) {
				switch (lt->first) {
				case HS::F001Blocked: {
					this->master->move_to(this->islabels[lt->first], this->states[lt->first],
						GraphletAnchor::LC, GraphletAnchor::RC, -gapsize);
				}; break;
				default: {
					this->master->move_to(this->islabels[lt->first], this->states[lt->first],
						GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
				}
				}
			}
		}
	}

private:
	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float radius, double degrees) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Silver);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, Colours::Silver);
			this->load_label(cs, id, Colours::Silver);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, "", this->number_font, this->unit_font), id);
		}
	}

	template<typename E>
	void load_boolean_indicators(E id0, E idn, float size, std::map<E, Credit<Booleanlet, E>*>& bs, std::map<E, Credit<Labellet, E>*>& ls) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<Booleanlet, E>(size, Colours::Green), id);
		}
	}

	template<class T, typename E>
	void load_thermometer(std::map<E, Credit<T, E>*>& ts, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float width) {
		ts[id] = this->master->insert_one(new Credit<T, E>(100.0, width, 0.0F, 2.5F), id);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>("celsius", _speak(id), "", this->number_font, this->unit_font), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ caption, E id
		, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(caption, font, color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

	template<typename E>
	Tanklet<E>* make_tank(E id, float width, float height, float thickness) {
		Tanklet<E>* tank = new Tanklet<E>(id, width, height, thickness);
		TankStyle ulow, low, normal;

		normal.mark_weight = 0.50F;
		low.mark_weight    = 0.20F;
		ulow.mark_weight   = 0.10F;
		
		tank->set_style(E::Normal,   normal);
		tank->set_style(E::Low,      low);
		tank->set_style(E::UltraLow, ulow);

		// WARNING: set styles before inserting 

		return this->master->insert_one(tank);
	}

// never deletes these graphlets mannually
private:
	Tracklet<HS>* station;
	FuelTanklet* storage_tank;
	Tanklet<HSMTStatus>* master_tank;
	Tanklet<HSVTStatus>* visor_tank;
	Dimensionlet* station_pressure;
	std::map<HS, Credit<Thermometerlet, HS>*> thermometers;
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Pumplet, HS>*> pumps;
	std::map<HS, Credit<Labellet, HS>*> plabels;
	std::map<HS, Credit<Labellet, HS>*> pcaptions;
	std::map<HS, Credit<Valvelet, HS>*> valves;
	std::map<HS, Credit<Labellet, HS>*> vlabels;
	std::map<HS, Credit<Dimensionlet, HS>*> pressures;
	std::map<HS, Credit<Dimensionlet, HS>*> temperatures;
	std::map<HS, Credit<Booleanlet, HS>*> states;
	std::map<HS, Credit<Labellet, HS>*> islabels;

	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ number_font;
	CanvasTextFormat^ unit_font;

private:
	HydraulicsPage* master;
};

HydraulicsPage::HydraulicsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Hydraulics* dashboard = new Hydraulics(this);

	this->dashboard = dashboard;
	this->operation = make_menu<HSOperation, IMRMaster*>(dashboard, plc);
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());

#ifdef _DEBUG
		this->append_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, this->gridsize));
#endif
	}
}

HydraulicsPage::~HydraulicsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void HydraulicsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HSMode::Dashboard);
			dashboard->load_pump_station(width, height, this->gridsize);
			dashboard->load_tanks(width, height, this->gridsize);
			dashboard->load_devices(width, height, this->gridsize);

			this->change_mode(HSMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		if (this->device != nullptr) {
			this->device->get_logger()->append_log_receiver(this->statusline);
		}
	}
}

void HydraulicsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(HSMode::Dashboard);
		dashboard->reflow_pump_station(width, height, this->gridsize, vinset);
		dashboard->reflow_devices(width, height, this->gridsize, vinset);
		dashboard->reflow_metrics(width, height, this->gridsize, vinset);
	}
}

bool HydraulicsPage::can_select(IGraphlet* g) {
	return (dynamic_cast<Pumplet*>(g) != nullptr);
}

void HydraulicsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
