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
#include "graphlet/svglet.hpp"

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
	Heater, Port, Starboard, Master, Visor, Storage,
	// Indicators
	F001Blocked, F002Blocked, LevelLow, LevelLow2, LevelHigh, FilterBlocked,
	_,
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, i, j, y, l, m, k,
	// anchors used for unnamed corners
	sq1, sqr
};

private class Hydraulics final : public PLCConfirmation, public IMenuCommand<HSOperation, IMRMaster*> {
public:
	Hydraulics(HydraulicsPage* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei UI", large_font_size);
		this->number_font = make_bold_text_format("Cambria Math", 20.0F);
		this->unit_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->temperatures[HS::Heater]->set_value(RealData(AI_DB203, 18U));
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
					this->bars[id]->set_value(RealData(AI_DB203, 8 + i), GraphletAnchor::RB);
				} else {
					this->bars[id]->set_value(RealData(AI_DB203, 8 + i));
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
		Turtle<HS>* pTurtle = new Turtle<HS>(gridsize, true, HS::SQ1);

		pTurtle->move_down(HS::sq1)->turn_down_right()->move_right(13, HS::l)->turn_right_down()->move_down(6);
		
		pTurtle->move_down(3, HS::f)->move_right(6, HS::SQf)->move_right(8, HS::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::c)->move_right(6, HS::SQc)->move_right(8, HS::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::d)->move_right(6, HS::SQd)->move_right(8, HS::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::e)->move_right(6, HS::SQe)->move_right(8, HS::E)->move_right(6);
		
		pTurtle->move_up(12, HS::Starboard)->move_up(20)->turn_up_left()->move_left(35)->turn_left_down()->move_down(4);
		pTurtle->jump_left(4)->move_up(4)->turn_up_left()->move_left(31)->turn_left_down()->move_down(20);

		pTurtle->move_down(3, HS::a)->move_right(6, HS::A)->move_right(8, HS::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::b)->move_right(6, HS::B)->move_right(8, HS::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::g)->move_right(6, HS::G)->move_right(8, HS::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::h)->move_right(6, HS::H)->move_right(8, HS::SQh)->move_right(6);

		pTurtle->move_up(12, HS::Port)->move_up(6, HS::F001Blocked)->turn_up_right()->move_right(13)->turn_right_up()->move_up(1, HS::SQ2);

		pTurtle->jump_back(HS::l);
		pTurtle->jump_left(5, HS::y)->turn_right_up()->move_up(4, HS::SQy)->move_up(4, HS::Y)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::l)->turn_right_up()->move_up(8, HS::L)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::m)->turn_right_up()->move_up(8, HS::M)->move_up(4)->jump_back();
		pTurtle->move_right(3, HS::SQk2)->move_right(3, HS::k)->move_up(9, HS::K)->move_up(3)->turn_up_left();
		pTurtle->move_left(21)->turn_left_down()->move_down(1.0F, HS::F002Blocked)->move_down(2.0F);

		pTurtle->jump_back(HS::k)->move_right(3, HS::SQk1)->move_right(2.5F, HS::Storage);

		pTurtle->jump_back(HS::SQ1)->jump_down(11.8F)->move_down(2, HS::SQi)->move_down(3, HS::I)->move_down(3);
		pTurtle->jump_back(HS::SQ2)->jump_down(11.8F)->move_down(2, HS::SQj)->move_down(3, HS::J)->move_down(3);
		
		pTurtle->jump_back(HS::SQ1)->move_up(2);
		pTurtle->jump_back(HS::SQ2)->move_up(2);
		
		this->station = this->master->insert_one(new Tracklet<HS>(pTurtle, 1.5F, Colours::Gray)); 

		this->load_label(this->captions, HS::Master, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Heater, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::Storage, Colours::Silver);
	}

	void load_tanks(float width, float height, float gridsize) {
		float thickness = 3.0F;

		this->master_tank = this->make_tank(HSMTStatus::Normal, gridsize * 14.0F, gridsize * 6.0F, thickness);
		this->visor_tank = this->make_tank(HSVTStatus::Normal, gridsize * 12.0F, gridsize * 5.0F, thickness);

		this->load_thermometer(this->thermometers, HS::Master, FitPosition::Left, gridsize * 2.5F);
		this->load_thermometer(this->thermometers, HS::Visor, FitPosition::Right, gridsize * 2.5F);

		this->storage_tank = new FuelTanklet(gridsize * 2.5F, 0.0F, thickness, Colours::WhiteSmoke);
		this->master->insert(this->storage_tank);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::A, HS::H, gridsize, 180.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::F, HS::E, gridsize, 0.000);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::Y, HS::K, gridsize, -90.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, HS::J, HS::I, gridsize, 90.00);

			this->load_dimensions(this->bars, HS::A, HS::I, "bar");
		}

		{ // load valves
			this->load_devices(this->valves, this->vlabels, HS::SQ1, HS::SQj, gridsize, 0.000);
			this->load_devices(this->valves, this->vlabels, HS::SQa, HS::SQk1, gridsize, -90.0);
			this->load_devices(this->valves, this->vlabels, HS::SQf, HS::SQe, gridsize, 90.00);
		}
	}

	void load_state_indicators(float width, float height, float gridsize) {
		float size = gridsize * 1.0F;

		this->load_status_indicator(HS::LevelLow, size, this->heater_states, this->hslabels);
		this->load_status_indicator(HS::LevelLow2, size, this->heater_states, this->hslabels);
		this->load_status_indicator(HS::LevelHigh, size, this->heater_states, this->hslabels);
		this->load_status_indicator(HS::F001Blocked, size, this->heater_states, this->hslabels);
		this->load_status_indicator(HS::F002Blocked, size, this->heater_states, this->hslabels);

		this->load_status_indicator(HS::LevelLow, size, this->visor_states, this->vslabels);
		this->load_status_indicator(HS::LevelLow2, size, this->visor_states, this->vslabels);
		this->load_status_indicator(HS::FilterBlocked, size, this->visor_states, this->vslabels);

		this->load_dimensions(this->temperatures, HS::Heater, HS::Visor, "celsius", "temperature");
	}

public:
	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float sw, sh, sx, sy, s1_x, s1_y;

		this->station->fill_extent(0.0F, 0.0F, &sw, &sh);
		this->station->fill_anchor_location(HS::SQ1, &s1_x, &s1_y);

		sx = (width - sw) * 0.5F;
		sy = (height - sh) * 0.5F - vinset * 0.5F;

		this->master->move_to(this->station, sx, sy);
		this->station->map_graphlet_at_anchor(this->storage_tank, HS::Storage, GraphletAnchor::LC);
		this->master->move_to(this->master_tank, sx + (sw - gridsize) * 0.5F, sy + s1_y - gridsize * 1.5F, GraphletAnchor::CB);
		this->master->move_to(this->visor_tank, sx + (sw - gridsize) * 0.5F, sy + s1_y + gridsize * 12.0F, GraphletAnchor::CB);
		this->master->move_to(this->thermometers[HS::Master], this->master_tank, GraphletAnchor::CC, GraphletAnchor::CC);
		this->master->move_to(this->thermometers[HS::Visor], this->visor_tank, GraphletAnchor::CC, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[HS::Port], GraphletAnchor::CB, -gridsize * 10.0F);
		this->station->map_credit_graphlet(this->captions[HS::Starboard], GraphletAnchor::CB, -gridsize * 10.0F);
		this->master->move_to(this->captions[HS::Storage], this->storage_tank, GraphletAnchor::CB, GraphletAnchor::CT);
		this->master->move_to(this->captions[HS::Heater], this->master_tank, GraphletAnchor::LB, GraphletAnchor::LT, gridsize);
		this->master->move_to(this->captions[HS::Visor], this->visor_tank, GraphletAnchor::CT, GraphletAnchor::CB, gridsize * 0.5F);
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

			if (this->bars.find(it->first) != this->bars.end()) {
				this->station->map_credit_graphlet(this->bars[it->first], bar_a, bar_dx, bar_dy);
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

	void reflow_state_indicators(float width, float height, float gridsize, float vinset) {
		this->master->move_to(this->captions[HS::Master], this->station, GraphletAnchor::LT, GraphletAnchor::CB, gridsize * 12.0F, gridsize * 4.0F);
		this->master->move_to(this->heater_states[HS::LevelLow], this->captions[HS::Master], GraphletAnchor::LB, GraphletAnchor::LT, -gridsize, gridsize);
		this->master->move_to(this->heater_states[HS::LevelLow2], this->heater_states[HS::LevelLow], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->heater_states[HS::LevelHigh], this->heater_states[HS::LevelLow2], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[HS::Heater], this->heater_states[HS::LevelHigh], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize);
		this->station->map_credit_graphlet(this->heater_states[HS::F001Blocked], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->heater_states[HS::F002Blocked], GraphletAnchor::CC);

		this->master->move_to(this->visor_states[HS::LevelLow], this->visor_tank, GraphletAnchor::RT, GraphletAnchor::LT, gridsize * 2.0F, 0.0F);
		this->master->move_to(this->visor_states[HS::LevelLow2], this->visor_states[HS::LevelLow], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->visor_states[HS::FilterBlocked], this->visor_states[HS::LevelLow2], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[HS::Visor], this->visor_states[HS::FilterBlocked], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize);

		{ // reflow state labels
			float gapsize = vinset * 0.25F;

			for (auto lt = this->heater_states.begin(); lt != this->heater_states.end(); lt++) {
				this->master->move_to(this->hslabels[lt->first], this->heater_states[lt->first],
					GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
			}

			for (auto lt = this->visor_states.begin(); lt != this->visor_states.end(); lt++) {
				this->master->move_to(this->vslabels[lt->first], this->visor_states[lt->first],
					GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
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
			this->load_label(ls, id, WarGrey::SCADA::Colours::Silver);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, WarGrey::SCADA::Colours::Silver);
			this->load_label(cs, id, WarGrey::SCADA::Colours::Silver);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, "", this->number_font, this->unit_font), id);
		}
	}

	template<typename E>
	void load_status_indicator(E id, float size, std::map<E, Credit<Booleanlet, E>*>& bs, std::map<E, Credit<Labellet, E>*>& ls) {
		this->load_label(ls, id, WarGrey::SCADA::Colours::Silver);
		bs[id] = this->master->insert_one(new Credit<Booleanlet, E>(size, Colours::Green), id);
	}

	template<class T, typename E>
	void load_thermometer(std::map<E, Credit<T, E>*>& ts, E id, FitPosition mp, float width) {
		ts[id] = this->master->insert_one(new Credit<T, E>(mp, 100.0F, 5U, width, 0.0F, 2.5F), id);
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
		return this->master->insert_one(new Tanklet<E>(id, width, height, thickness));
	}

// never deletes these graphlets mannually
private:
	Tracklet<HS>* station;
	FuelTanklet* storage_tank;
	Tanklet<HSMTStatus>* master_tank;
	Tanklet<HSVTStatus>* visor_tank;
	std::map<HS, Credit<Thermometerlet, HS>*> thermometers;
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Pumplet, HS>*> pumps;
	std::map<HS, Credit<Labellet, HS>*> plabels;
	std::map<HS, Credit<Labellet, HS>*> pcaptions;
	std::map<HS, Credit<Valvelet, HS>*> valves;
	std::map<HS, Credit<Labellet, HS>*> vlabels;
	std::map<HS, Credit<Dimensionlet, HS>*> bars;
	std::map<HS, Credit<Dimensionlet, HS>*> temperatures;
	std::map<HS, Credit<Booleanlet, HS>*> heater_states;
	std::map<HS, Credit<Labellet, HS>*> hslabels;
	std::map<HS, Credit<Booleanlet, HS>*> visor_states;
	std::map<HS, Credit<Labellet, HS>*> vslabels;
	
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
			dashboard->load_state_indicators(width, height, this->gridsize);

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
		dashboard->reflow_state_indicators(width, height, this->gridsize, vinset);
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
