#include <map>

#include "page/loads.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/tagged_valvelet.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum LDMode { WindowUI = 0, Dashboard };

private enum class LDGVOperation { Open, Close, FakeOpen, FakeClose, _ };
private enum class LDMVOperation { Open, Close, FakeOpen, FakeClose, Heat, _ };

// WARNING: order matters
private enum class LD : unsigned int {
	Port, Starboard,
	
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024, D026,
	D003, D007, D011, D023, D025,
	D004, D005, D009, D012,
	D014, D016,
	D013, D015,

	// Pump Dimensions
	A, C, F, H,
	
	// Key Labels
	PSUWPump, SBUWPump, PSHPump, SBHPump, Gantry, LMOD,
	
	_,
	// anchors used as last jumping points
	d11, d12, d13, d14, d24,
	d0225, d0326, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, gantry, deck_lx, deck_rx, deck_ty, deck_by,

	// anchors used for non-interconnected nodes
	n0325, n0405, n0723, n0923
};

private class Barge final
	: public PLCConfirmation
	, public IMenuCommand<LDGVOperation, Credit<GateValvelet, LD>, IMRMaster*>
	, public IMenuCommand<LDMVOperation, Credit<MotorValvelet, LD>, IMRMaster*> {
public:
	Barge(LoadsPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->dimensions[LD::C]->set_value(RealData(AI_DB203, 8U), GraphletAnchor::LB);
		this->dimensions[LD::F]->set_value(RealData(AI_DB203, 9U), GraphletAnchor::LT);

		this->dimensions[LD::A]->set_value(RealData(AI_DB203, 12U), GraphletAnchor::LB);
		this->dimensions[LD::H]->set_value(RealData(AI_DB203, 15U), GraphletAnchor::LT);

		this->percentages[LD::D003]->set_value(RealData(AI_DB203, 39U), GraphletAnchor::LB);
		//this->percentages[LD::D004]->set_value(RealData(AI_DB203, 15U), GraphletAnchor::LT);
	}

	void on_raw_digital_input(const uint8* DI_DB4, size_t count, WarGrey::SCADA::Syslog* logger) override {
		
		this->set_hopper_pump_status(LD::PSHPump, LD::PSUWPump, DI_DB4, 1U);
		this->set_hopper_pump_status(LD::SBHPump, LD::SBUWPump, DI_DB4, 25U);
		
		this->set_pump_dimension_status(LD::A, DI_DB4, 50U);
		this->set_pump_dimension_status(LD::C, DI_DB4, 58U);
		this->set_pump_dimension_status(LD::F, DI_DB4, 74U);
		this->set_pump_dimension_status(LD::H, DI_DB4, 66U);

		this->set_valve_status(LD::D005, DI_DB4, 259U, 417U); // PS Isolation
		this->set_valve_status(LD::D006, DI_DB4, 261U, 419U); // PS Underwater Unload
		this->set_valve_status(LD::D002, DI_DB4, 273U, 421U); // Empty
		this->set_valve_status(LD::D003, DI_DB4, 279U, 423U); // SB Suction <==
		this->set_valve_status(LD::D004, DI_DB4, 257U, 425U); // PS Suction
		this->set_valve_status(LD::D016, DI_DB4, 375U, 427U); // PS Back Unload
		this->set_valve_status(LD::D015, DI_DB4, 407U, 429U); // SB Back Unload
		this->set_valve_status(LD::D014, DI_DB4, 373U, 431U); // PS Fore Unload
		this->set_valve_status(LD::D013, DI_DB4, 405U, 433U); // SB Fore Unload
		this->set_valve_status(LD::D024, DI_DB4, 413U, 435U); // Gantry
		this->set_valve_status(LD::D012, DI_DB4, 333U, 437U); // PS LMOD Unload
		this->set_valve_status(LD::D010, DI_DB4, 295U, 439U); // PS Main Load
		this->set_valve_status(LD::D011, DI_DB4, 349U, 441U); // SB LMOD Unload
		this->set_valve_status(LD::D009, DI_DB4, 293U, 443U); // PS Underwater Load
		this->set_valve_status(LD::D017, DI_DB4, 297U, 445U); // PS Hopper Load
		this->set_valve_status(LD::D020, DI_DB4, 303U, 447U); // PS Shore Unload
		this->set_valve_status(LD::D021, DI_DB4, 305U, 449U); // Bow Fill
		this->set_valve_status(LD::D019, DI_DB4, 301U, 451U); // SB Shore Unload
		this->set_valve_status(LD::D018, DI_DB4, 299U, 453U); // SB Hopper Load
		this->set_valve_status(LD::D007, DI_DB4, 289U, 455U); // SB Underwater Load
		this->set_valve_status(LD::D008, DI_DB4, 291U, 457U); // SB Main Load
		this->set_valve_status(LD::D023, DI_DB4, 309U, 459U); // Through
		this->set_valve_status(LD::D022, DI_DB4, 307U, 461U); // Bow Jet
		this->set_valve_status(LD::D026, DI_DB4, 277U, 463U); // SB Underwater Unload
		//this->set_valve_status(LD::D001, DI_DB4, 239U, 465U); // Empty and Water
		this->set_valve_status(LD::D025, DI_DB4, 275U, 467U); // SB Isolation
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(LDGVOperation cmd, Credit<GateValvelet, LD>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"Gate Valve: %s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

	void execute(LDMVOperation cmd, Credit<MotorValvelet, LD>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"Motor Valve: %s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->special_font = make_text_format(tiny_font_size);
		this->pump_style = make_highlight_dimension_style(large_font_size, 6U, Colours::Background);
		this->highlight_style = make_highlight_dimension_style(large_font_size, 6U, Colours::Green);
		this->relationship_style = make_dash_stroke(CanvasDashStyle::DashDot);
		this->relationship_color = Colours::DarkGray;
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		Turtle<LD>* pTurtle = new Turtle<LD>(gwidth, gheight, false);

		pTurtle->move_left(LD::deck_rx)->move_left(2, LD::D021)->move_left(2, LD::d2122);
		pTurtle->move_down(5)->move_right(2, LD::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, LD::d1920)->move_left(2, LD::D020)->move_left(6, LD::d1720);

		pTurtle->move_left(3, LD::D017)->move_left(11, LD::n0405)->move_left(4, LD::D010)->move_left(6, LD::d12);
		pTurtle->move_down(2, LD::D012)->move_down(3)->jump_down(LD::LMOD)->jump_back(LD::d12);
		pTurtle->move_left(4, LD::d14)->move_left_down(2, LD::D014)->move_left_down(1.5F)->jump_back();
		pTurtle->move_left(5, LD::d24)->move_left(4)->move_left_down(2, LD::D016)->move_left_down(1.5F)->jump_back();
		pTurtle->jump_up(2.5F, LD::gantry)->turn_up_left()->move_left(3, LD::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, LD::Gantry)->move_left()->jump_back(LD::Gantry)->move_right()->jump_back(LD::d1720);
		
		pTurtle->move_down(3.5F, LD::PSHPump)->move_left(6, LD::n0923)->move_left(8)->move_up(1.5F, LD::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, LD::d0406)->move_right(4, LD::D006)->move_right(4)->move_down(0.5F, LD::deck_ty)->move_down(LD::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, LD::D023)->jump_back(LD::d0406);

		pTurtle->move_up(1.5F, LD::D004)->move_up(2, LD::ps)->move_up(2, LD::C)->turn_up_left();
		pTurtle->move_left(10, LD::PSUWPump)->move_left(8, LD::A)->move_left(LD::Port);

		pTurtle->jump_back(LD::D023)->move_down(2)->jump_down()->move_down(5, LD::D007);
		pTurtle->move_down(LD::deck_by)->move_down(0.5F)->move_left(4, LD::D026)->move_left(4, LD::d0326);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, LD::D025)->move_up(1.5F, LD::d0225);
		pTurtle->move_right(8, LD::n0723)->move_right(6, LD::SBHPump)->move_down(3.5F, LD::d1819)->jump_back(LD::d0225);
		pTurtle->move_up(2.5F)->move_left(2, LD::D002)->move_left(28, LD::D001)->move_left(2)->jump_back(LD::d1819);

		pTurtle->move_left(3, LD::D018)->move_left(11, LD::n0325)->move_left(4, LD::D008);
		pTurtle->move_left(6, LD::d11)->move_up(2, LD::D011)->move_up(3)->jump_back();
		pTurtle->move_left(4, LD::d13)->move_left_up(2, LD::D013)->move_left_up(1.5F)->jump_back();
		pTurtle->move_left(9)->move_left_up(2, LD::D015)->move_left_up(1.5F);

		pTurtle->jump_back(LD::d0326)->move_down(1.5F, LD::D003)->move_down(2, LD::sb)->move_down(2, LD::F)->turn_down_left();
		pTurtle->move_left(10, LD::SBUWPump)->move_left(8, LD::H)->move_left(LD::Starboard);

		pTurtle->jump_back(LD::d1819)->move_right(4, LD::deck_lx)->move_right(2, LD::D019)->move_right(2)->move_to(LD::d1920);
		
		this->station = this->master->insert_one(new Tracklet<LD>(pTurtle, default_pipe_thickness, default_pipe_color));

		{ // load valves
			this->load_valve(this->gvalves, this->vlabels, this->captions, LD::D001, radius, 0.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D002, LD::D026, radius, 00.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D003, LD::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D004, LD::D012, radius, -90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D014, LD::D016, radius, -45.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D013, LD::D015, radius, 45.0);
		}

		{ // load special nodes
			float nic_radius = gheight * 0.25F;

			this->load_pump(this->hoppers, this->captions, LD::PSUWPump, -radius, -2.0F);
			this->load_pump(this->hoppers, this->captions, LD::SBUWPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, LD::PSHPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, LD::SBHPump, -radius, -2.0F);

			this->LMOD = this->master->insert_one(new Arclet(0.0, 360.0, gheight, gheight, 0.5F, default_pipe_color));

			this->ps_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_ps_color, default_pipe_thickness));

			this->sb_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_sb_color, default_pipe_thickness));

			for (LD id = LD::n0325; id <= LD::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipe_thickness, default_pipe_color));
			}
		}

		{ // load labels and dimensions
			this->load_percentage(this->percentages, LD::D003);
			this->load_percentage(this->percentages, LD::D004);
			this->load_dimensions(this->dimensions, LD::A, LD::H, "bar");

			this->load_label(this->captions, LD::Gantry, Colours::Yellow, this->caption_font);
			this->load_label(this->captions, LD::LMOD, Colours::Yellow, this->special_font);
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin, label_height, ox, oy;
		float gridsize = std::fminf(gwidth, gheight);
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[LD::Gantry], GraphletAnchor::CB);
		this->station->map_credit_graphlet(this->captions[LD::LMOD], GraphletAnchor::CB);

		this->station->map_graphlet_at_anchor(this->ps_draghead, LD::Port, GraphletAnchor::RC);
		this->station->map_graphlet_at_anchor(this->sb_draghead, LD::Starboard, GraphletAnchor::RC);
		this->station->map_graphlet_at_anchor(this->LMOD, LD::LMOD, GraphletAnchor::CC);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->station->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipe_thickness * 0.5F);
		}

		for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
			switch (it->first) {
			case LD::PSUWPump: { anchor = GraphletAnchor::LB; }; break;
			case LD::SBUWPump: { anchor = GraphletAnchor::LT; }; break;
			default: { anchor = GraphletAnchor::LC; }
			}

			it->second->fill_pump_origin(&ox);
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, -ox);
			this->master->move_to(this->captions[it->first],it->second,
				GraphletAnchor::RC, anchor, std::fabsf(ox));
		}

		this->vlabels[LD::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case LD::D014: case LD::D016: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D013: case LD::D015: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D006: case LD::D010: case LD::D020: case LD::D021: case LD::D022: case LD::D024: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
				dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
			}; break;
			case LD::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case LD::D001: case LD::D002: case LD::D008: case LD::D019: case LD::D026: {
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

		{ // reflow motor-driven valves
			float polar45 = gridsize * std::sqrtf(2.0F) * 0.618F;

			for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
				switch (it->first) {
				case LD::D014: case LD::D016: {
					dx = x0 - polar45; dy = y0 - polar45; anchor = GraphletAnchor::CC;
				}; break;
				case LD::D013: case LD::D015: {
					dx = x0 - polar45; dy = y0 + polar45; anchor = GraphletAnchor::CC;
				}; break;
				case LD::D003: case LD::D004: case LD::D005: case LD::D007: case LD::D009:
				case LD::D011: case LD::D012: case LD::D023: case LD::D025: {
					this->gvalves[LD::D003]->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
					dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RC;
				}; break;
				case LD::D002: case LD::D008: case LD::D017: case LD::D019: case LD::D026: {
					dx = x0; dy = y0 + gridsize; anchor = GraphletAnchor::CC;
				}; break;
				default: {
					dx = x0; dy = y0 - gridsize; anchor = GraphletAnchor::CC;
				}
				}

				it->second->fill_valve_origin(&ox, &oy);
				this->station->map_credit_graphlet(it->second, anchor, dx - ox, dy - oy);
			}
		}

		{ // reflow dimensions
			float offset = default_pipe_thickness * 2.0F;

			this->master->move_to(this->percentages[LD::D003], this->gvalves[LD::D003], GraphletAnchor::CB, GraphletAnchor::LT, offset, -offset);
			this->master->move_to(this->percentages[LD::D004], this->gvalves[LD::D004], GraphletAnchor::CT, GraphletAnchor::LB, offset);

			this->station->map_credit_graphlet(this->dimensions[LD::A], GraphletAnchor::LB, 0.0F, -offset);
			this->station->map_credit_graphlet(this->dimensions[LD::C], GraphletAnchor::LB, gwidth);
			this->station->map_credit_graphlet(this->dimensions[LD::F], GraphletAnchor::LT, gwidth);
			this->station->map_credit_graphlet(this->dimensions[LD::H], GraphletAnchor::LT, 0.0F, +offset);
		}
	}

public:
	void draw_valves_relationships(CanvasDrawingSession^ ds, float Width, float Height) {
		float ox, oy, gx, gy, mx, my;

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			this->master->fill_graphlet_location(it->second, &mx, &my, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->gvalves[it->first], &gx, &gy, GraphletAnchor::CC);
			it->second->fill_valve_origin(&ox, &oy);

			ds->DrawLine(mx + ox, my + oy, gx, gy, this->relationship_color, 1.0F, this->relationship_style);
		}
	}

private:
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
			case LD::D014: case LD::D016: mdegrees = 45.0; break;
			case LD::D013: case LD::D015: mdegrees = -45.0; break;
			case LD::D002: case LD::D008: case LD::D009: case LD::D017: case LD::D019: case LD::D026: mdegrees = -180.0; break;
			}

			// moter-driven valves' second, catching events first 
			this->load_valve(gs, ls, cs, id, radius, degrees);
			ms[id] = this->master->insert_one(new M(mradius, mdegrees, false), id);
		}
	}

	template<class G, typename E>
	void load_pump(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon, this->caption_font);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->percentage_style), id);
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
	void set_hopper_pump_status(LD id, const uint8* db4, size_t idx, bool on) {
		if (on) {
			HopperPumplet* target = this->hoppers[id];

			target->set_remote_control(DBX(db4, idx + 3));

			target->set_status(DBX(db4, idx + 0), HopperPumpStatus::Ready);
			target->set_status(DBX(db4, idx + 4), HopperPumpStatus::Alert);
			target->set_status(DBX(db4, idx + 5), HopperPumpStatus::Broken);
			target->set_status(DBX(db4, idx + 6), HopperPumpStatus::Running);
			target->set_status(DBX(db4, idx + 7), HopperPumpStatus::Maintenance);
		}
	}

	void set_hopper_pump_status(LD id1, LD id2, const uint8* db4, size_t idx_p1) {
		bool hopper = DBX(db4, idx_p1 + 0);
		bool underw = DBX(db4, idx_p1 + 1);

		this->set_hopper_pump_status(id1, db4, idx_p1 - 1, hopper);
		this->set_hopper_pump_status(id2, db4, idx_p1 - 1, underw);
	}

	void set_valve_status(LD id, const uint8* db4, size_t gidx_p1, size_t midx_p1) {
		this->gvalves[id]->set_status(DBX(db4, gidx_p1 - 1), GateValveStatus::Open, GateValveStatus::Closed);
		this->mvalves[id]->set_status(DBX(db4, midx_p1 - 1), TValveStatus::Open, TValveStatus::Closed);
	}

	void set_pump_dimension_status(LD id, const uint8* db4, size_t idx_p1) {
		this->dimensions[id]->set_status(DBX(db4, idx_p1 - 1) ? DimensionStatus::Highlight : DimensionStatus::Normal);
	}

// never deletes these graphlets mannually
private:
	Tracklet<LD>* station;
	std::map<LD, Credit<Labellet, LD>*> captions;
	std::map<LD, Credit<HopperPumplet, LD>*> hoppers;
	std::map<LD, Credit<GateValvelet, LD>*> gvalves;
	std::map<LD, Credit<MotorValvelet, LD>*> mvalves;
	std::map<LD, Credit<Labellet, LD>*> vlabels;
	std::map<LD, Credit<Percentagelet, LD>*> percentages;
	std::map<LD, Credit<Dimensionlet, LD>*> dimensions;
	std::map<LD, Omegalet*> nintercs;
	Segmentlet* ps_draghead;
	Segmentlet* sb_draghead;
	Arclet* LMOD;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ special_font;
	ICanvasBrush^ relationship_color;
	CanvasStrokeStyle^ relationship_style;
	DimensionStyle pump_style;
	DimensionStyle highlight_style;
	DimensionStyle percentage_style;

private:
	LoadsPage* master;
};

private class ShipDecorator : public IPlanetDecorator {
public:
	ShipDecorator(Barge* master) : master(master) {
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
		this->master->draw_valves_relationships(ds, Width, Height);
	}

	void draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool is_selected) override {
		auto station = dynamic_cast<Tracklet<LD>*>(g);

		if (station != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;
			
			station->fill_anchor_location(LD::ps, nullptr, &ps_y, false);
			station->fill_anchor_location(LD::sb, nullptr, &sb_y, false);

			station->fill_anchor_location(LD::deck_lx, &deck_lx, nullptr, false);
			station->fill_anchor_location(LD::deck_rx, &deck_rx, nullptr, false);
			station->fill_anchor_location(LD::deck_ty, nullptr, &deck_ty, false);
			station->fill_anchor_location(LD::deck_by, nullptr, &deck_by, false);

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
				float gantry_x, gantry_y, barge_y;
				float d0525_x, d05_y, d25_y;

				station->fill_anchor_location(LD::gantry, &gantry_x, &gantry_y, false);
				station->fill_anchor_location(LD::D008, nullptr, &barge_y, false);
				station->fill_anchor_location(LD::D005, &d0525_x, &d05_y, false);
				station->fill_anchor_location(LD::D025, nullptr, &d25_y, false);

				ds->DrawLine(x + gantry_x, y + gantry_y, x + gantry_x, y + barge_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(x + d0525_x, y + d05_y, x + d0525_x, y + d25_y,
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
	Barge* master;
};

LoadsPage::LoadsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Barge* dashboard = new Barge(this);

	this->dashboard = dashboard;
	this->gate_valve_op = make_menu<LDGVOperation, Credit<GateValvelet, LD>, IMRMaster*>(dashboard, plc);
	this->motor_valve_op = make_menu<LDMVOperation, Credit<MotorValvelet, LD>, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new ShipDecorator(dashboard));

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

LoadsPage::~LoadsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void LoadsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Barge*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(LDMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(LDMode::WindowUI);
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

void LoadsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Barge*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(LDMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(LDMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool LoadsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<GateValvelet*>(g) != nullptr) || (dynamic_cast<MotorValvelet*>(g) != nullptr));
}

void LoadsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	auto mvalve = dynamic_cast<MotorValvelet*>(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (mvalve != nullptr) {
		menu_popup(this->motor_valve_op, g, local_x, local_y);
	}
}
