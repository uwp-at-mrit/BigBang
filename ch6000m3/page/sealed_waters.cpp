#include <map>

#include "page/sealed_waters.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/misc/hatchlet.hpp"
#include "graphlet/symbol/pumplet.hpp"
#include "graphlet/symbol/valvelet.hpp"
#include "graphlet/device/tanklet.hpp"
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

private enum SWMode { WindowUI = 0, Dashboard };

private enum class SWOperation { Start, Stop, Reset, _ };
private enum class HSMTStatus { Empty, UltraLow, Low, Normal, High, Full, _ };
private enum class HSVTStatus { Empty, UltraLow, Low, Normal, Full, _ };

// WARNING: order matters
private enum class SW : unsigned int {
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
	Port, Starboard, Master, Visor, Hatch, Pressure,
	// Indicators
	F001Blocked, F002Blocked,
	TS1, TS2, TS11, TS12,
	FilterBlocked,
	_,
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, i, j, y, l, m, k,

	// anchors used for unnamed anchors
};

private class SealedWaters final : public PLCConfirmation, public IMenuCommand<SWOperation, IMRMaster*> {
public:
	SealedWaters(SealedWaterPage* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei UI", large_font_size);
		this->number_font = make_bold_text_format("Cambria Math", 20.0F);
		this->unit_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->temperatures[SW::Master]->set_value(RealData(AI_DB203, 18U));
		this->temperatures[SW::Visor]->set_value(RealData(AI_DB203, 19U));

		{ // pump pressures
			static std::map<SW, bool> need_lock_position;
			SW bar_seq[] = { SW::C, SW::F, SW::D, SW::E, SW::A, SW::B, SW::G, SW::H, SW::I, SW::J };

			if (need_lock_position.size() == 0) {
				need_lock_position[SW::F] = true;
				need_lock_position[SW::C] = true;
				need_lock_position[SW::D] = true;
				need_lock_position[SW::E] = true;
				need_lock_position[SW::J] = true;
			}

			for (size_t i = 0; i < sizeof(bar_seq) / sizeof(SW); i++) {
				SW id = bar_seq[i];
				
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
			SW pump_seq[] = { SW::A, SW::B, SW::C, SW::D, SW::E, SW::G, SW::G, SW::H, SW::Y, SW::K, SW::L, SW::M, SW::I, SW::J };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(SW); i++) {
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
	void execute(SWOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto pump = dynamic_cast<Credit<Pumplet, SW>*>(target);

		if (pump != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				pump->id.ToString()->Data());
		}
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<SW>* pTurtle = new Turtle<SW>(gridsize, true, SW::Master);

		pTurtle->move_right(2)->move_down(5.5F, SW::SQ1);
		pTurtle->move_down()->turn_down_right()->move_right(13, SW::l)->turn_right_down()->move_down(5);
		
		pTurtle->move_down(3, SW::f)->move_right(6, SW::SQf)->move_right(8, SW::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::c)->move_right(6, SW::SQc)->move_right(8, SW::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::d)->move_right(6, SW::SQd)->move_right(8, SW::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::e)->move_right(6, SW::SQe)->move_right(8, SW::E)->move_right(6);
		
		pTurtle->move_up(11, SW::Starboard)->move_up(22)->turn_up_left()->move_left(35);
		pTurtle->turn_left_down()->move_down(2, SW::F001Blocked)->move_down(2);
		pTurtle->jump_up(4)->turn_up_left()->move_left(35)->turn_left_down()->move_down(21);

		pTurtle->move_down(3, SW::a)->move_right(6, SW::A)->move_right(8, SW::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::b)->move_right(6, SW::B)->move_right(8, SW::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::g)->move_right(6, SW::G)->move_right(8, SW::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, SW::h)->move_right(6, SW::H)->move_right(8, SW::SQh)->move_right(6);

		pTurtle->move_up(12, SW::Port)->move_up(5)->turn_up_right()->move_right(13)->turn_right_up()->move_up(1, SW::SQ2);
		pTurtle->move_up(5.5F)->move_right(2 /* SW::Master */);

		pTurtle->jump_back(SW::l);
		pTurtle->jump_left(5, SW::y)->turn_right_up()->move_up(4, SW::SQy)->move_up(4, SW::Y)->move_up(6)->jump_back();
		pTurtle->move_right(5, SW::l)->turn_right_up()->move_up(4, SW::SQl)->move_up(4, SW::L)->move_up(6)->jump_back();
		pTurtle->move_right(5, SW::m)->turn_right_up()->move_up(4, SW::SQm)->move_up(4, SW::M)->move_up(6)->jump_back();
		pTurtle->move_right(3, SW::SQk2)->move_right(3, SW::k)->move_up(9, SW::K)->move_up(5)->turn_up_left();
		pTurtle->move_left(21)->turn_left_down()->move_down(SW::F002Blocked)->move_down(2);

		pTurtle->jump_back(SW::k)->move_right(3, SW::SQk1)->move_right(2.5F, SW::Hatch);
		
		pTurtle->jump_back(SW::Master)->jump_down(14, SW::Visor);
		pTurtle->move_right(2)->move_down(5, SW::SQi)->move_down(3, SW::I)->move_down(3);
		pTurtle->jump_left(4)->move_up(3, SW::J)->move_up(3, SW::SQj)->move_up(5)->move_right(2 /* SW::Visor */);
		
		this->station = this->master->insert_one(new Tracklet<SW>(pTurtle, 1.5F, Colours::Gray));
		
		this->load_label(this->captions, SW::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, SW::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, SW::Hatch, Colours::Silver);
	}

	void load_tanks(float width, float height, float gridsize) {
		float thickness = 3.0F;

		this->master_tank = this->make_tank(HSMTStatus::High, gridsize * 17.0F, gridsize * 8.0F, thickness);
		this->visor_tank = this->make_tank(HSVTStatus::Normal, gridsize * 15.0F, gridsize * 6.0F, thickness);

		this->load_thermometer(this->thermometers, this->temperatures, SW::Master, gridsize * 2.5F);
		this->load_thermometer(this->thermometers, this->temperatures, SW::Visor, gridsize * 2.5F);

		this->hatchdoor = new Hatchlet(gridsize * 2.5F, 0.0F, thickness);
		this->master->insert(this->hatchdoor);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_devices(this->pumps, this->plabels, this->pcaptions, SW::A, SW::H, gridsize, 180.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, SW::F, SW::E, gridsize, 0.000);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, SW::Y, SW::K, gridsize, -90.0);
			this->load_devices(this->pumps, this->plabels, this->pcaptions, SW::J, SW::I, gridsize, 90.00);

			this->load_dimensions(this->pressures, SW::A, SW::I, "bar");

			this->station_pressure = new Dimensionlet("bar", _speak(SW::Pressure), "", this->number_font, this->unit_font);
			this->master->insert(this->station_pressure);
		}

		{ // load valves
			this->load_devices(this->valves, this->vlabels, SW::SQ1, SW::SQj, gridsize, 0.000);
			this->load_devices(this->valves, this->vlabels, SW::SQa, SW::SQk1, gridsize, -90.0);
			this->load_devices(this->valves, this->vlabels, SW::SQf, SW::SQe, gridsize, 90.00);
		}
	}

public:
	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float sq1_y;

		this->master->move_to(this->station, cx, cy, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->hatchdoor, SW::Hatch, GraphletAnchor::LC);
		this->station->fill_anchor_location(SW::SQ1, nullptr, &sq1_y, true);
		this->station->map_graphlet_at_anchor(this->master_tank, SW::Master, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->visor_tank, SW::Visor, GraphletAnchor::CC);
		this->master->move_to(this->station_pressure, this->station, GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->thermometers[SW::Master], this->master_tank, 0.25F, 0.5F, GraphletAnchor::CC);
		this->master->move_to(this->thermometers[SW::Visor], this->visor_tank, 0.25F, 0.5F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[SW::Port], GraphletAnchor::CB, -gridsize * 10.0F);
		this->station->map_credit_graphlet(this->captions[SW::Starboard], GraphletAnchor::CB, -gridsize * 10.0F);
		this->master->move_to(this->captions[SW::Hatch], this->hatchdoor, GraphletAnchor::CB, GraphletAnchor::CT);
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
			case SW::A: case SW::B: case SW::G: case SW::H: {
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
				cpt_dx = x0 + gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::LT;
				bar_dx = x0 + gridsize; bar_dy = y0; bar_a = GraphletAnchor::LB;
			} break;
			case SW::F: case SW::C: case SW::D: case SW::E: {
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
				cpt_dx = x0 - gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::RT;
				bar_dx = x0 - gridsize; bar_dy = y0; bar_a = GraphletAnchor::RB;
			} break;
			case SW::Y: case SW::L: case SW::M: case SW::K: {
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RB;
				cpt_dx = x0 + text_hspace; cpt_dy = y0 - gridsize; cpt_a = GraphletAnchor::LB;
				bar_dx = x0; bar_dy = y0; bar_a = GraphletAnchor::CC; // these devices have no metrics
			} break;
			default: {
				cpt_dx = x0; cpt_dy = y0 + gridsize * 3.0F; cpt_a = GraphletAnchor::CT;
			
				if (it->second->id == SW::I) {
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
				case SW::SQ2: case SW::SQy: {
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

		this->master->move_to(this->temperatures[SW::Master], this->thermometers[SW::Master], GraphletAnchor::RB, GraphletAnchor::LB, gridsize);
		this->master->move_to(this->temperatures[SW::Visor], this->thermometers[SW::Visor], GraphletAnchor::RB, GraphletAnchor::LB, gridsize);
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
	Tracklet<SW>* station;
	Hatchlet* hatchdoor;
	Tanklet<HSMTStatus>* master_tank;
	Tanklet<HSVTStatus>* visor_tank;
	Dimensionlet* station_pressure;
	std::map<SW, Credit<Thermometerlet, SW>*> thermometers;
	std::map<SW, Credit<Labellet, SW>*> captions;
	std::map<SW, Credit<Pumplet, SW>*> pumps;
	std::map<SW, Credit<Labellet, SW>*> plabels;
	std::map<SW, Credit<Labellet, SW>*> pcaptions;
	std::map<SW, Credit<Valvelet, SW>*> valves;
	std::map<SW, Credit<Labellet, SW>*> vlabels;
	std::map<SW, Credit<Dimensionlet, SW>*> pressures;
	std::map<SW, Credit<Dimensionlet, SW>*> temperatures;
	std::map<SW, Credit<Labellet, SW>*> islabels;

	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ number_font;
	CanvasTextFormat^ unit_font;

private:
	SealedWaterPage* master;
};

SealedWaterPage::SealedWaterPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	SealedWaters* dashboard = new SealedWaters(this);

	this->dashboard = dashboard;
	this->operation = make_menu<SWOperation, IMRMaster*>(dashboard, plc);
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());

#ifdef _DEBUG
		this->append_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, this->gridsize));
#endif
	}
}

SealedWaterPage::~SealedWaterPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void SealedWaterPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(SWMode::Dashboard);
			dashboard->load_pump_station(width, height, this->gridsize);
			dashboard->load_tanks(width, height, this->gridsize);
			dashboard->load_devices(width, height, this->gridsize);

			this->change_mode(SWMode::WindowUI);
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

void SealedWaterPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(SWMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(SWMode::Dashboard);
		dashboard->reflow_pump_station(width, height, this->gridsize, vinset);
		dashboard->reflow_devices(width, height, this->gridsize, vinset);
		dashboard->reflow_metrics(width, height, this->gridsize, vinset);
	}
}

bool SealedWaterPage::can_select(IGraphlet* g) {
	return (dynamic_cast<Pumplet*>(g) != nullptr);
}

void SealedWaterPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
