#include <map>

#include "page/fill_jet.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/door/hatchlet.hpp"
#include "graphlet/symbol/door/hopper_doorlet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/tagged_valvelet.hpp"

#include "schema/di_pump_dimensions.hpp"
#include "schema/di_hopper_pumps.hpp"
#include "schema/di_valves.hpp"
#include "schema/di_doors.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum FJMode { WindowUI = 0, Dashboard };

private enum class FJGVOperation { Open, Close, FakeOpen, FakeClose, _ };
private enum class FJMVOperation { Open, Close, FakeOpen, FakeClose, Heat, _ };
private enum class FJHDOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class FJ : unsigned int {
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024,
	D003, D007, D023, D025,
	D004, D005, D009,

	// Pump dimensions
	A, C, F, H,

	// Hopper doors
	SB1, SB2, SB3, SB4, SB5, SB6, SB7,
	PS1, PS2, PS3, PS4, PS5, PS6, PS7,

	// Key Labels
	Port, Starboard, Hatch, PSHPump, SBHPump, Gantry,

	// Settings
	PSPC, SBPC, PSFC, SBFC,
	
	_,
	// anchors used as last jumping points
	d0225, d0325, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, d007, deck_lx, deck_rx, deck_ty, deck_by,

	// anchors used for non-interconnected nodes
	n24, n0325, n0405, n0723, n0923
};

private class FillJet final
	: public PLCConfirmation
	, public IMenuCommand<FJGVOperation, Credit<GateValvelet, FJ>, IMRMaster*>
	, public IMenuCommand<FJMVOperation, Credit<MotorValvelet, FJ>, IMRMaster*>
	, public IMenuCommand<FJHDOperation, Credit<UpperHopperDoorlet, FJ>, IMRMaster*> {
public:
	FillJet(FillnJetPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->powers[FJ::PSHPump]->set_value(DBD(DB2, 12U));
		this->powers[FJ::SBHPump]->set_value(DBD(DB2, 16U));
		//this->powers[FJ::PSUWPump]->set_value(DBD(DB2, 200U));
		//this->powers[FJ::SBUWPump]->set_value(DBD(DB2, 204U));

		//this->rpms[FJ::PSHPump]->set_value(DBD(DB2, 604U));
		//this->rpms[FJ::SBHPump]->set_value(DBD(DB2, 608U));
		//this->rpms[FJ::PSUWPump]->set_value(DBD(DB2, 200U));
		//this->rpms[FJ::SBUWPump]->set_value(DBD(DB2, 204U));
	}

	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		this->pressures[FJ::C]->set_value(RealData(DB203, 8U), GraphletAnchor::LB);
		this->pressures[FJ::F]->set_value(RealData(DB203, 9U), GraphletAnchor::LT);

		this->pressures[FJ::A]->set_value(RealData(DB203, 12U), GraphletAnchor::LB);
		this->pressures[FJ::H]->set_value(RealData(DB203, 15U), GraphletAnchor::LT);

		this->progresses[FJ::D003]->set_value(RealData(DB203, 39U), GraphletAnchor::LB);
		//this->progresses[FJ::D004]->set_value(RealData(DB203, 15U), GraphletAnchor::LT);

		{ // door progresses
			this->set_door_progress(FJ::PS1, RealData(DB203, 53U));
			this->set_door_progress(FJ::PS2, RealData(DB203, 54U));
			this->set_door_progress(FJ::PS3, RealData(DB203, 55U));
			this->set_door_progress(FJ::PS4, RealData(DB203, 77U));
			this->set_door_progress(FJ::PS5, RealData(DB203, 78U));
			this->set_door_progress(FJ::PS6, RealData(DB203, 79U));
			this->set_door_progress(FJ::PS7, RealData(DB203, 80U));

			this->set_door_progress(FJ::SB1, RealData(DB203, 69U));
			this->set_door_progress(FJ::SB2, RealData(DB203, 70U));
			this->set_door_progress(FJ::SB3, RealData(DB203, 71U));
			this->set_door_progress(FJ::SB4, RealData(DB203, 93U));
			this->set_door_progress(FJ::SB5, RealData(DB203, 94U));
			this->set_door_progress(FJ::SB6, RealData(DB203, 95U));
			this->set_door_progress(FJ::SB7, RealData(DB203, 96U));
		}
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, WarGrey::SCADA::Syslog* logger) override {
		DI_hopper_pump(this->hoppers[FJ::PSHPump], DB4, 1U, DB205, 857U);
		DI_hopper_pump(this->hoppers[FJ::SBHPump], DB4, 25U, DB205, 873U);

		DI_pump_dimension(this->pressures[FJ::A], DB4, 50U);
		DI_pump_dimension(this->pressures[FJ::C], DB4, 58U);
		DI_pump_dimension(this->pressures[FJ::F], DB4, 74U);
		DI_pump_dimension(this->pressures[FJ::H], DB4, 66U);

		{ // Missing DB4 info
			DI_hopper_door(this->uhdoors[FJ::PS1], DB4, 329U, DB205, 1089U);
			DI_hopper_door(this->uhdoors[FJ::PS2], DB4, 330U, DB205, 1105U);
			DI_hopper_door(this->uhdoors[FJ::PS3], DB4, 331U, DB205, 1121U);
			DI_hopper_door(this->uhdoors[FJ::PS4], DB4, 369U, DB205, 1137U);
			DI_hopper_door(this->uhdoors[FJ::PS5], DB4, 370U, DB205, 1153U);
			DI_hopper_door(this->uhdoors[FJ::PS6], DB4, 371U, DB205, 1169U);
			DI_hopper_door(this->uhdoors[FJ::PS7], DB4, 372U, DB205, 1185U);

			DI_hopper_door(this->uhdoors[FJ::SB1], DB4, 345U, DB205, 1097U);
			DI_hopper_door(this->uhdoors[FJ::SB2], DB4, 346U, DB205, 1113U);
			DI_hopper_door(this->uhdoors[FJ::SB3], DB4, 347U, DB205, 1129U);
			DI_hopper_door(this->uhdoors[FJ::SB4], DB4, 401U, DB205, 1145U);
			DI_hopper_door(this->uhdoors[FJ::SB5], DB4, 402U, DB205, 1161U);
			DI_hopper_door(this->uhdoors[FJ::SB6], DB4, 403U, DB205, 1177U);
			DI_hopper_door(this->uhdoors[FJ::SB7], DB4, 404U, DB205, 1193U);
		}
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(FJGVOperation cmd, Credit<GateValvelet, FJ>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"Gate Valve: %s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

	void execute(FJMVOperation cmd, Credit<MotorValvelet, FJ>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"Motor Valve: %s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

	void execute(FJHDOperation cmd, Credit<UpperHopperDoorlet, FJ>* door, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			door->id.ToString()->Data());
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->pump_style = make_highlight_dimension_style(metrics_font_size, 6U, Colours::Background);
		this->highlight_style = make_highlight_dimension_style(metrics_font_size, 6U, Colours::Green);
		this->setting_style = make_setting_dimension_style(metrics_font_size, 6U);
		this->relationship_style = make_dash_stroke(CanvasDashStyle::DashDot);
		this->relationship_color = Colours::DarkGray;
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		Turtle<FJ>* pTurtle = new Turtle<FJ>(gwidth, gheight, false);

		pTurtle->move_left(FJ::deck_rx)->move_left(2, FJ::D021)->move_left(2, FJ::d2122);
		pTurtle->move_down(5)->move_right(2, FJ::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, FJ::d1920)->move_left(2, FJ::D020)->move_left(7, FJ::d1720);

		pTurtle->move_left(3, FJ::D017)->move_left(11, FJ::n0405)->move_left(4, FJ::D010)->jump_back(FJ::d1720);
		
		pTurtle->move_down(3.5F, FJ::PSHPump)->move_left(6, FJ::n0923)->move_left(8)->move_up(1.5F, FJ::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, FJ::d0406)->move_right(4, FJ::D006)->move_right(4)->move_down(0.5F, FJ::deck_ty)->move_down(FJ::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, FJ::D023)->jump_back(FJ::d0406);

		pTurtle->move_up(1.5F, FJ::D004)->move_up(2, FJ::ps)->move_up(2, FJ::C)->move_up(FJ::Port);

		pTurtle->jump_back(FJ::D023)->move_down(2)->jump_down()->move_down(5, FJ::D007);
		pTurtle->move_down(FJ::deck_by)->move_down(0.5F, FJ::d007)->jump_left(8, FJ::d0325);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, FJ::D025)->move_up(1.5F, FJ::d0225);
		pTurtle->move_right(8, FJ::n0723)->move_right(6, FJ::SBHPump)->move_down(3.5F, FJ::d1819)->jump_back(FJ::d0225);
		pTurtle->jump_up(2.5F)->move_left(2, FJ::D002)->move_left(15, FJ::n24)->move_left(10, FJ::D001)->move_left(3, FJ::Hatch);

		pTurtle->jump_back(FJ::d1819)->move_left(3, FJ::D018)->move_left(11, FJ::n0325)->move_left(4, FJ::D008);
		pTurtle->move_left(13)->move_up(5.5F)->jump_up()->move_up(6.5F);
		pTurtle->move_up(2.5F)->turn_up_left()->move_left(3, FJ::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, FJ::Gantry)->move_left()->jump_back(FJ::Gantry)->move_right()->jump_back(FJ::d0325);

		pTurtle->move_down(1.5F, FJ::D003)->move_down(2, FJ::sb)->move_down(2, FJ::F)->move_down(FJ::Starboard);

		pTurtle->jump_back(FJ::d1819)->move_right(5, FJ::deck_lx)->move_right(2, FJ::D019)->move_right(2)->move_to(FJ::d1920);
		
		this->station = this->master->insert_one(new Tracklet<FJ>(pTurtle, default_pipe_thickness, default_pipe_color));

		{ // load manual pipe segement
			float d02_y, d05_y;

			this->station->fill_anchor_location(FJ::D002, nullptr, &d02_y);
			this->station->fill_anchor_location(FJ::D005, nullptr, &d05_y);

			this->manual_pipe = this->master->insert_one(
				new Linelet(0.0F, d02_y, 0.0F, d05_y,
					default_pipe_thickness, default_pipe_color));
		}

		{ // load doors and valves
			this->load_doors(this->uhdoors, this->progresses, FJ::PS1, FJ::PS7, radius);
			this->load_doors(this->uhdoors, this->progresses, FJ::SB1, FJ::SB7, radius);
		
			this->load_valve(this->gvalves, this->vlabels, this->captions, FJ::D001, radius, 0.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D002, FJ::D024, radius, 00.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D003, FJ::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D004, FJ::D009, radius, -90.0);
		}

		{ // load special nodes
			float sct_radius = radius * 0.5F;
			float nic_radius = radius * 0.25F;
			
			this->load_pump(this->hoppers, this->captions, FJ::PSHPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, FJ::SBHPump, -radius, -2.0F);
			this->ps_suction = this->master->insert_one(new Circlelet(sct_radius, default_ps_color, default_pipe_thickness));
			this->sb_suction = this->master->insert_one(new Circlelet(sct_radius, default_sb_color, default_pipe_thickness));
			this->sea_inlet = this->master->insert_one(new Hatchlet(radius * 2.0F));

			for (FJ id = FJ::n24; id <= FJ::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipe_thickness, default_pipe_color));
			}
		}

		{ // load labels and dimensions
			this->load_percentage(this->progresses, FJ::D003);
			this->load_percentage(this->progresses, FJ::D004);
			this->load_dimensions(this->pressures, FJ::A, FJ::H, "bar");

			this->load_setting(this->dsettings, FJ::PSPC, "bar");
			this->load_setting(this->dsettings, FJ::SBPC, "bar");

			this->load_setting(this->psettings, FJ::PSFC);
			this->load_setting(this->psettings, FJ::SBFC);

			this->load_label(this->captions, FJ::Port, Colours::make(default_ps_color), this->caption_font);
			this->load_label(this->captions, FJ::Starboard, Colours::make(default_sb_color), this->caption_font);
			this->load_label(this->captions, FJ::Hatch, Colours::SeaGreen, this->caption_font);
			this->load_label(this->captions, FJ::Gantry, Colours::Yellow, this->caption_font);

			for (size_t idx = 0; idx < hopper_count; idx++) {
				this->sequences[idx] = this->master->insert_one(new Labellet((idx + 1).ToString() + "#"));
				this->sequences[idx]->set_font(this->caption_font);
				this->sequences[idx]->set_color(Colours::Silver);
			}
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin, label_height, ox, oy;
		float gridsize = resolve_gridsize(gwidth, gheight);
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->manual_pipe, FJ::D025, GraphletAnchor::CB);

		this->station->map_credit_graphlet(this->captions[FJ::Gantry], GraphletAnchor::CB);
		this->station->map_graphlet_at_anchor(this->ps_suction, FJ::Port, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->sb_suction, FJ::Starboard, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->sea_inlet, FJ::Hatch, GraphletAnchor::CC);
		this->master->move_to(this->captions[FJ::Hatch], this->sea_inlet, GraphletAnchor::CB, GraphletAnchor::CT);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->station->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipe_thickness * 0.5F);
		}

		this->reflow_doors(this->uhdoors, this->progresses, FJ::PS1, FJ::PS7, gheight * -2.5F);
		this->reflow_doors(this->uhdoors, this->progresses, FJ::SB1, FJ::SB7, gheight * +2.5F);

		for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
			it->second->fill_pump_origin(&ox);
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, -ox);
			this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LB, std::fabsf(ox));
			this->master->move_to(this->powers[it->first], this->captions[it->first], GraphletAnchor::LB, GraphletAnchor::LT);
			this->master->move_to(this->rpms[it->first], this->powers[it->first], GraphletAnchor::LB, GraphletAnchor::LT);
		}

		this->vlabels[FJ::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case FJ::D006: case FJ::D010: case FJ::D020: case FJ::D021: case FJ::D022: case FJ::D024: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
				dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
			}; break;
			case FJ::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case FJ::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case FJ::D001: case FJ::D002: case FJ::D008: case FJ::D019: {
				it->second->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
				dx = x0; dy = y0 - gridsize - label_height + margin; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				it->second->fill_margin(x0, y0, nullptr, &margin, nullptr, nullptr);
				dx = x0 + gridsize - margin; dy = y0; anchor = GraphletAnchor::LB;
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->captions[it->first], anchor, dx, dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			switch (it->first) {
			case FJ::D003: case FJ::D004: case FJ::D005: case FJ::D007: case FJ::D009:
			case FJ::D023: case FJ::D025: {
				this->gvalves[FJ::D003]->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
				dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			case FJ::D002: case FJ::D008: case FJ::D017: case FJ::D019: {
				dx = x0; dy = y0 + gridsize; anchor = GraphletAnchor::CC;
			}; break;
			default: {
				dx = x0; dy = y0 - gridsize; anchor = GraphletAnchor::CC;
			}
			}

			it->second->fill_valve_origin(&ox, &oy);
			this->station->map_credit_graphlet(it->second, anchor, dx - ox, dy - oy);
		}

		{ // reflow door sequences
			this->station->fill_anchor_location(FJ::D008, nullptr, &y0);
			for (unsigned int idx = 0; idx < hopper_count; idx++) {
				this->master->fill_graphlet_location(this->uhdoors[_E(FJ, idx + _I(FJ::PS1))], &x0, nullptr, GraphletAnchor::CC);
				this->master->move_to(this->sequences[idx], x0, y0, GraphletAnchor::CB);
			}
		}

		{ // reflow settings dimensions
			float soff = gwidth * 24.0F;
			float doff = gwidth * 18.0F;
			float gap = default_pipe_thickness * 2.0F;

			this->station->map_graphlet_at_anchor(this->captions[FJ::Port], FJ::ps, GraphletAnchor::RB, -soff, -gheight);
			this->master->move_to(this->psettings[FJ::PSFC], this->captions[FJ::Port], GraphletAnchor::RB, GraphletAnchor::LB, vinset);
			this->master->move_to(this->dsettings[FJ::PSPC], this->psettings[FJ::PSFC], GraphletAnchor::RB, GraphletAnchor::LB, vinset);
			
			this->station->map_graphlet_at_anchor(this->captions[FJ::Starboard], FJ::sb, GraphletAnchor::RT, -soff, gheight);
			this->master->move_to(this->psettings[FJ::SBFC], this->captions[FJ::Starboard], GraphletAnchor::RT, GraphletAnchor::LT, vinset);
			this->master->move_to(this->dsettings[FJ::SBPC], this->psettings[FJ::SBFC], GraphletAnchor::RT, GraphletAnchor::LT, vinset);
			
			this->master->move_to(this->progresses[FJ::D003], this->gvalves[FJ::D003], GraphletAnchor::CB, GraphletAnchor::LT, gap, -gap);
			this->master->move_to(this->progresses[FJ::D004], this->gvalves[FJ::D004], GraphletAnchor::CT, GraphletAnchor::LB, gap);
			
			this->station->map_graphlet_at_anchor(this->pressures[FJ::A], FJ::Port, GraphletAnchor::LB, -doff);
			this->station->map_credit_graphlet(this->pressures[FJ::C], GraphletAnchor::LB, gwidth);
			this->station->map_credit_graphlet(this->pressures[FJ::F], GraphletAnchor::LT, gwidth);
			this->station->map_graphlet_at_anchor(this->pressures[FJ::H], FJ::Starboard, GraphletAnchor::LT, -doff);
		}
	}

public:
	void draw_relationships(CanvasDrawingSession^ ds, float Width, float Height) {
		float ox, oy, sx, sy, tx, ty;

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			this->master->fill_graphlet_location(it->second, &sx, &sy, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->gvalves[it->first], &tx, &ty, GraphletAnchor::CC);
			it->second->fill_valve_origin(&ox, &oy);

			ds->DrawLine(sx + ox, sy + oy, tx, ty, this->relationship_color, 1.0F, this->relationship_style);
		}

		for (unsigned int idx = 0; idx < hopper_count; idx++) {
			this->master->fill_graphlet_location(this->uhdoors[_E(FJ, idx + _I(FJ::PS1))], &sx, &sy, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->uhdoors[_E(FJ, idx + _I(FJ::SB1))], &tx, &ty, GraphletAnchor::CC);
			
			ds->DrawLine(sx, sy, tx, ty, this->relationship_color, 1.0F, this->relationship_style);
		}
	}

private:
	template<typename E>
	void load_setting(std::map<E, Credit<Percentagelet, E>*>& ds, E id) {
		ds[id] = this->master->insert_one(new Credit<Percentagelet, E>(DimensionStatus::Input, this->setting_style, _speak(id)), id);
	}

	template<typename E>
	void load_setting(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(DimensionStatus::Input, this->setting_style, unit, _speak(id)), id);
	}

	template<class G, typename E>
	void load_valve(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, std::map<E, Credit<Labellet, E>*>& cs
		, E id, float radius, double degrees) {
		this->load_label(ls, "(" + id.ToString() + ")", id, Colours::Silver, this->label_font);
		this->load_label(cs, id, Colours::Silver, this->label_font);

		gs[id] = this->master->insert_one(new G(radius, degrees), id);
	}
	
	template<class G, class M, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, M*>& ms, std::map<E, Credit<Labellet, E>*>& ls
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
		float mradius = radius * 0.8F;

		for (E id = id0; id <= idn; id++) {
			double mdegrees = 0.0;

			switch (id) {
			case FJ::D002: case FJ::D008: case FJ::D009: case FJ::D017: case FJ::D019: mdegrees = -180.0; break;
			}

			// moter-driven valves' second, catching events first 
			this->load_valve(gs, ls, cs, id, radius, degrees);
			ms[id] = this->master->insert_one(new M(mradius, mdegrees, false), id);
		}
	}

	template<class D, typename E>
	void load_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<D, E>(radius), id);
			this->load_percentage(ps, id);
		}
	}

	template<class G, typename E>
	void load_pump(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon, this->caption_font);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);
		this->powers[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, "kwatt"), id);
		this->rpms[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, "rpm"), id);
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->plain_style), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, id.ToString()), id);

			ds[id]->set_style(DimensionStatus::Normal, this->pump_style);
			ds[id]->set_style(DimensionStatus::Highlight, this->highlight_style);
		}
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

private:
	template<class D, typename E>
	void reflow_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float yoff) {
		GraphletAnchor d_anchor = GraphletAnchor::CT;
		GraphletAnchor p_anchor = GraphletAnchor::CB;
		float lx, rx, y, cell_width;
		float label_yoff = default_pipe_thickness * 2.0F;
		
		if (yoff > 0.0F) { // Starboard
			d_anchor = GraphletAnchor::CB;
			p_anchor = GraphletAnchor::CT;
		}

		this->station->fill_anchor_location(FJ::D001, &lx, &y);
		this->station->fill_anchor_location(FJ::D010, &rx, nullptr);
		cell_width = (rx - lx) / float(hopper_count);

		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;
			float x = lx + cell_width * (0.5F + float(hopper_count - idx));
			
			this->master->move_to(ds[id], x, y + yoff, GraphletAnchor::CC);
			this->master->move_to(ps[id], ds[id], d_anchor, p_anchor);
		}
	}

private:
	void set_door_progress(FJ id, float value) {
		this->uhdoors[id]->set_value(value / 100.0F);
		this->progresses[id]->set_value(value, GraphletAnchor::CC);
	}

	void set_valve_status(FJ id, const uint8* db4, size_t gidx_p1, size_t midx_p1) {
		this->gvalves[id]->set_status(DBX(db4, gidx_p1 - 1), GateValveStatus::Open, GateValveStatus::Closed);
		this->mvalves[id]->set_status(DBX(db4, midx_p1 - 1), TValveStatus::Open, TValveStatus::Closed);
	}

// never deletes these graphlets mannually
private:
	Tracklet<FJ>* station;
	std::map<FJ, Credit<Labellet, FJ>*> captions;
	std::map<FJ, Credit<HopperPumplet, FJ>*> hoppers;
	std::map<FJ, Credit<GateValvelet, FJ>*> gvalves;
	std::map<FJ, Credit<MotorValvelet, FJ>*> mvalves;
	std::map<FJ, Credit<Labellet, FJ>*> vlabels;
	std::map<FJ, Credit<UpperHopperDoorlet, FJ>*> uhdoors;
	std::map<FJ, Credit<Percentagelet, FJ>*> progresses;
	std::map<FJ, Credit<Dimensionlet, FJ>*> dsettings;
	std::map<FJ, Credit<Percentagelet, FJ>*> psettings;
	std::map<FJ, Credit<Dimensionlet, FJ>*> pressures;
	std::map<FJ, Credit<Dimensionlet, FJ>*> powers;
	std::map<FJ, Credit<Dimensionlet, FJ>*> rpms;
	Labellet* sequences[hopper_count];
	std::map<FJ, Omegalet*> nintercs;
	Linelet* manual_pipe;
	Hatchlet* sea_inlet;
	Circlelet* ps_suction;
	Circlelet* sb_suction;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	ICanvasBrush^ relationship_color;
	CanvasStrokeStyle^ relationship_style;
	DimensionStyle pump_style;
	DimensionStyle highlight_style;
	DimensionStyle setting_style;
	DimensionStyle plain_style;

private:
	FillnJetPage* master;
};

private class FillnJetDecorator : public IPlanetDecorator {
public:
	FillnJetDecorator(FillJet* master) : master(master) {
		float height = 1.0F;
		float xradius = height * 0.10F;
		float yradius = height * 0.50F;

		this->ship_width = 1.0F - xradius;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, yradius, -90.0, 90.0, xradius, yradius));

		this->ship_style = make_dash_stroke(CanvasDashStyle::Dash);
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		this->master->draw_relationships(ds, Width, Height);
	}

	void draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool is_selected) override {
		auto station = dynamic_cast<Tracklet<FJ>*>(g);

		if (station != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;

			station->fill_anchor_location(FJ::ps, nullptr, &ps_y, false);
			station->fill_anchor_location(FJ::sb, nullptr, &sb_y, false);

			station->fill_anchor_location(FJ::deck_lx, &deck_lx, nullptr, false);
			station->fill_anchor_location(FJ::deck_rx, &deck_rx, nullptr, false);
			station->fill_anchor_location(FJ::deck_ty, nullptr, &deck_ty, false);
			station->fill_anchor_location(FJ::deck_by, nullptr, &deck_by, false);

			{ // draw ship
				float ship_width = this->actual_width();
				float ship_height = std::fabsf(sb_y - ps_y);
				auto real_ship = geometry_scale(this->ship, ship_width, ship_height);
				Rect ship_box = real_ship->ComputeBounds();
				float sx = 0.0F;
				float sy = y + std::fminf(sb_y, ps_y);

				ds->DrawGeometry(real_ship, sx, sy, Colours::SeaGreen, 1.0F, this->ship_style);
			}

			{ // draw deck region
				float dx = x + std::fminf(deck_lx, deck_rx);
				float dy = y + std::fminf(deck_ty, deck_by);
				float dw = std::fabsf((deck_rx - deck_lx));
				float dh = std::fabsf((deck_by - deck_ty));

				ds->DrawGeometry(rectangle(dx, dy, dw, dh), Colours::SeaGreen, 1.0F, this->ship_style);
			}


			{ // draw non-important lines
				float d0525_x, d05_y, d25_y;
				float d0325_y, d03_x, d07_x;
				float d10_x, d10_y;

				station->fill_anchor_location(FJ::D005, &d0525_x, &d05_y, false);
				station->fill_anchor_location(FJ::D025, nullptr, &d25_y, false);
				station->fill_anchor_location(FJ::D010, &d10_x, &d10_y, false);
				station->fill_anchor_location(FJ::d0325, &d03_x, &d0325_y, false);
				station->fill_anchor_location(FJ::d007, &d07_x, nullptr, false);

				ds->DrawLine(x + d0525_x, y + d05_y, x + d0525_x, y + d25_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(x + d03_x, y + d0325_y, x + d07_x, y + d0325_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(d10_x, y + d10_y, x + d10_x, y + d10_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);
			}
		}
	}

private:
	CanvasGeometry^ ship;
	CanvasStrokeStyle^ ship_style;

private:
	float ship_width;

private:
	FillJet* master;
};

FillnJetPage::FillnJetPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	FillJet* dashboard = new FillJet(this);

	this->dashboard = dashboard;
	this->gate_valve_op = make_menu<FJGVOperation, Credit<GateValvelet, FJ>, IMRMaster*>(dashboard, plc);
	this->motor_valve_op = make_menu<FJMVOperation, Credit<MotorValvelet, FJ>, IMRMaster*>(dashboard, plc);
	this->upper_door_op = make_menu<FJHDOperation, Credit<UpperHopperDoorlet, FJ>, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new FillnJetDecorator(dashboard));

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

FillnJetPage::~FillnJetPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void FillnJetPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<FillJet*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(FJMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(FJMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			this->get_logger()->append_log_receiver(this->statusline);
			
			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void FillnJetPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<FillJet*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(FJMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(FJMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool FillnJetPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<GateValvelet*>(g) != nullptr)
		|| (dynamic_cast<MotorValvelet*>(g) != nullptr)
		|| (dynamic_cast<UpperHopperDoorlet*>(g) != nullptr));
}

void FillnJetPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	auto mvalve = dynamic_cast<MotorValvelet*>(g);
	auto uhdoor = dynamic_cast<UpperHopperDoorlet*>(g);
	auto editor = dynamic_cast<IEditorlet*>(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (mvalve != nullptr) {
		menu_popup(this->motor_valve_op, g, local_x, local_y);
	} else if (uhdoor != nullptr) {
		menu_popup(this->upper_door_op, g, local_x, local_y);
	} else if (editor != nullptr) {
		if (editor->get_status() == DimensionStatus::Input) {
			this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		}
	}
}
