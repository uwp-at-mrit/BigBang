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

static ICanvasBrush^ block_color = Colours::Firebrick;
static ICanvasBrush^ nonblock_color = Colours::WhiteSmoke;

// WARNING: order matters
private enum class FJ : unsigned int {
	Port, Starboard,
	
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024,
	D003, D007, D023, D025,
	D004, D005, D009,

	// Upper Hopper Doors
	SB1, SB2, SB3, SB4, SB5, SB6, SB7,
	PS1, PS2, PS3, PS4, PS5, PS6, PS7,

	// Key Labels
	Hatch, PSHPump, SBHPump, Gantry,
	
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
	, public IMenuCommand<FJGVOperation, IMRMaster*>
	, public IMenuCommand<FJMVOperation, IMRMaster*>
	, public IMenuCommand<FJHDOperation, IMRMaster*> {
public:
	FillJet(FillnJetPage* master) : master(master) {}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(FJGVOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto valve = dynamic_cast<Credit<GateValvelet, FJ>*>(target);

		if (valve != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"Gate Valve: %s %s",
				cmd.ToString()->Data(),
				valve->id.ToString()->Data());
		}
	}

	void execute(FJMVOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto valve = dynamic_cast<Credit<MotorValvelet, FJ>*>(target);

		if (valve != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"Motor Valve: %s %s",
				cmd.ToString()->Data(),
				valve->id.ToString()->Data());
		}
	}

	void execute(FJHDOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto door = dynamic_cast<Credit<UpperHopperDoorlet, FJ>*>(target);

		if (door != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				door->id.ToString()->Data());
		}
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", 14.0F);
		this->label_font = make_bold_text_format("Microsoft YaHei", 10.0F);
		this->relationship_style = make_dash_stroke(CanvasDashStyle::DashDot);
		this->relationship_color = Colours::DarkGray;

		this->dimension_style = make_highlight_dimension_style(gheight, 5U);
		this->percentage_style.unit_color = Colours::Silver;
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<FJ>* pTurtle = new Turtle<FJ>(gwidth, gheight, false);

		pTurtle->move_left(FJ::deck_rx)->move_left(2, FJ::D021)->move_left(2, FJ::d2122);
		pTurtle->move_down(5)->move_right(2, FJ::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, FJ::d1920)->move_left(2, FJ::D020)->move_left(6, FJ::d1720);

		pTurtle->move_left(3, FJ::D017)->move_left(11, FJ::n0405)->move_left(3, FJ::D010)->jump_back(FJ::d1720);
		
		pTurtle->move_down(3.5F, FJ::PSHPump)->move_left(6, FJ::n0923)->move_left(8)->move_up(1.5F, FJ::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, FJ::d0406)->move_right(4, FJ::D006)->move_right(4)->move_down(0.5F, FJ::deck_ty)->move_down(FJ::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, FJ::D023)->jump_back(FJ::d0406);

		pTurtle->move_up(1.5F, FJ::D004)->move_up(2, FJ::ps)->move_up(3, FJ::Port);

		pTurtle->jump_back(FJ::D023)->move_down(2)->jump_down()->move_down(5, FJ::D007);
		pTurtle->move_down(FJ::deck_by)->move_down(0.5F, FJ::d007)->jump_left(8, FJ::d0325);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, FJ::D025)->move_up(1.5F, FJ::d0225);
		pTurtle->move_right(8, FJ::n0723)->move_right(6, FJ::SBHPump)->move_down(3.5F, FJ::d1819)->jump_back(FJ::d0225);
		pTurtle->jump_up(2.5F)->move_left(2, FJ::D002)->move_left(15, FJ::n24)->move_left(10, FJ::D001)->move_left(3, FJ::Hatch);

		pTurtle->jump_back(FJ::d1819)->move_left(3, FJ::D018)->move_left(11, FJ::n0325)->move_left(3, FJ::D008);
		pTurtle->move_left(14)->move_up(5.5F)->jump_up()->move_up(5.5F);
		pTurtle->move_up(2.5F)->turn_up_left()->move_left(3, FJ::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, FJ::Gantry)->move_left()->jump_back(FJ::Gantry)->move_right()->jump_back(FJ::d0325);

		pTurtle->move_down(1.5F, FJ::D003)->move_down(2, FJ::sb)->move_down(3, FJ::Starboard);

		pTurtle->jump_back(FJ::d1819)->move_right(4, FJ::deck_lx)->move_right(2, FJ::D019)->move_right(2)->move_to(FJ::d1920);
		
		this->pipeline = this->master->insert_one(new Tracklet<FJ>(pTurtle, default_pipeline_thickness, default_pipeline_color));

		{ // load manual pipe segement
			float d02_y, d05_y;

			this->pipeline->fill_anchor_location(FJ::D002, nullptr, &d02_y);
			this->pipeline->fill_anchor_location(FJ::D005, nullptr, &d05_y);

			this->manual_pipe = this->master->insert_one(
				new Linelet(0.0F, d02_y, 0.0F, d05_y,
					default_pipeline_thickness, default_pipeline_color));
		}

		{ // load doors
			float radius = std::fminf(gwidth, gheight);

			this->load_doors(this->uhdoors, this->progresses, FJ::PS1, FJ::PS7, radius);
			this->load_doors(this->uhdoors, this->progresses, FJ::SB1, FJ::SB7, radius);
		}

		{ // load valves
			float radius = std::fminf(gwidth, gheight);

			this->load_valve(this->gvalves, this->vlabels, this->captions, FJ::D001, radius, 0.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D002, FJ::D024, radius, 00.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D003, FJ::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, FJ::D004, FJ::D009, radius, -90.0);
		}

		{ // load special nodes
			float radius = std::fminf(gwidth, gheight);
			float nic_radius = radius * 0.5F;
			
			this->load_pumps(this->pumps, this->captions, FJ::PSHPump, FJ::SBHPump, radius);
			this->ps_suction = this->master->insert_one(new Circlelet(nic_radius, default_port_color, default_pipeline_thickness));
			this->sb_suction = this->master->insert_one(new Circlelet(nic_radius, default_starboard_color, default_pipeline_thickness));
			this->sea_inlet = this->master->insert_one(new Hatchlet(radius * 2.0F));

			for (FJ id = FJ::n24; id <= FJ::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipeline_thickness, default_pipeline_color));
			}
		}

		{ // load other labels
			this->load_label(this->captions, FJ::Hatch, Colours::SeaGreen, this->caption_font);
			this->load_label(this->captions, FJ::Gantry, Colours::Yellow, this->caption_font);

			for (size_t idx = 0; idx < hopper_count; idx++) {
				this->sequences[idx] = this->master->insert_one(new Labellet((hopper_count - idx).ToString() + "#"));
				this->sequences[idx]->set_font(this->caption_font);
				this->sequences[idx]->set_color(Colours::Tomato);
			}
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy;
		float gridsize = std::fminf(gwidth, gheight);
		float adjust_gridsize = gridsize * 0.618F;
		float label_height;
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->manual_pipe, FJ::D025, GraphletAnchor::CB);

		this->pipeline->map_credit_graphlet(this->captions[FJ::Gantry], GraphletAnchor::CB);
		this->pipeline->map_graphlet_at_anchor(this->ps_suction, FJ::Port, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->sb_suction, FJ::Starboard, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->sea_inlet, FJ::Hatch, GraphletAnchor::CC);
		this->master->move_to(this->captions[FJ::Hatch], this->sea_inlet, GraphletAnchor::CB, GraphletAnchor::CT);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->pipeline->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipeline_thickness * 0.5F);
		}

		this->reflow_doors(this->uhdoors, this->progresses, FJ::PS1, FJ::PS7, gridsize * -2.4F);
		this->reflow_doors(this->uhdoors, this->progresses, FJ::SB1, FJ::SB7, gridsize * +2.4F);

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC);
			this->pipeline->map_credit_graphlet(this->captions[it->first], GraphletAnchor::LC, gridsize, 0.0F);
		}

		this->vlabels[FJ::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case FJ::D006: case FJ::D010: case FJ::D020: case FJ::D021: case FJ::D022: case FJ::D024: {
				dx = x0; dy = y0 + adjust_gridsize; anchor = GraphletAnchor::CT;
			}; break;
			case FJ::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case FJ::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case FJ::D001: case FJ::D002: case FJ::D008: case FJ::D019: {
				dx = x0; dy = y0 - adjust_gridsize - label_height; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				dx = x0 + adjust_gridsize; dy = y0; anchor = GraphletAnchor::LB;
			}
			}

			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->pipeline->map_credit_graphlet(this->captions[it->first], anchor, dx, dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}

		{ // reflow motor-driven valves
			float ox, oy;

			for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
				switch (it->first) {
				case FJ::D003: case FJ::D004: case FJ::D005: case FJ::D007: case FJ::D009:
				case FJ::D023: case FJ::D025: {
					dx = x0 - adjust_gridsize; dy = y0; anchor = GraphletAnchor::RC;
				}; break;
				case FJ::D002: case FJ::D008: case FJ::D017: case FJ::D019: {
					dx = x0; dy = y0 + gridsize; anchor = GraphletAnchor::CC;
				}; break;
				default: {
					dx = x0; dy = y0 - gridsize; anchor = GraphletAnchor::CC;
				}
				}

				it->second->fill_valve_origin(&ox, &oy);
				this->pipeline->map_credit_graphlet(it->second, anchor, dx - ox, dy - oy);
			}
		}

		{ // reflow door sequences
			this->pipeline->fill_anchor_location(FJ::D010, nullptr, &y0);
			for (unsigned int idx = 0; idx < hopper_count; idx++) {
				this->master->fill_graphlet_location(this->uhdoors[_E(FJ, idx + _I(FJ::PS1))], &x0, nullptr, GraphletAnchor::CC);
				this->master->move_to(this->sequences[idx], x0, y0, GraphletAnchor::CT);
			}
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
		float mradius = radius * 0.618F;

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
			ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->percentage_style), id);
		}
	}

	template<class G, typename E>
	void load_pumps(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Salmon, this->caption_font);

			gs[id] = this->master->insert_one(new G(-radius, 0.0F, 0.0), id);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->dimension_style, unit, label), id);
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
		
		if (yoff > 0.0F) { // Starboard
			d_anchor = GraphletAnchor::CB;
			p_anchor = GraphletAnchor::CT;
		}

		this->pipeline->fill_anchor_location(FJ::D001, &lx, &y);
		this->pipeline->fill_anchor_location(FJ::D010, &rx, nullptr);
		cell_width = (rx - lx) / float(hopper_count);

		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;
			float x = lx + cell_width * (0.5F + float(hopper_count - idx));
			
			this->master->move_to(ds[id], x, y + yoff, GraphletAnchor::CC);
			this->master->move_to(ps[id], ds[id], d_anchor, p_anchor);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<FJ>* pipeline;
	std::map<FJ, Credit<Labellet, FJ>*> captions;
	std::map<FJ, Credit<UpperHopperDoorlet, FJ>*> uhdoors;
	std::map<FJ, Credit<Percentagelet, FJ>*> progresses;
	std::map<FJ, Credit<HopperPumplet, FJ>*> pumps;
	std::map<FJ, Credit<GateValvelet, FJ>*> gvalves;
	std::map<FJ, Credit<MotorValvelet, FJ>*> mvalves;
	std::map<FJ, Credit<Labellet, FJ>*> vlabels;
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
	DimensionStyle dimension_style;
	DimensionStyle percentage_style;

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
		auto pipeline = dynamic_cast<Tracklet<FJ>*>(g);

		if (pipeline != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;

			pipeline->fill_anchor_location(FJ::ps, nullptr, &ps_y, false);
			pipeline->fill_anchor_location(FJ::sb, nullptr, &sb_y, false);

			pipeline->fill_anchor_location(FJ::deck_lx, &deck_lx, nullptr, false);
			pipeline->fill_anchor_location(FJ::deck_rx, &deck_rx, nullptr, false);
			pipeline->fill_anchor_location(FJ::deck_ty, nullptr, &deck_ty, false);
			pipeline->fill_anchor_location(FJ::deck_by, nullptr, &deck_by, false);

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

				pipeline->fill_anchor_location(FJ::D005, &d0525_x, &d05_y, false);
				pipeline->fill_anchor_location(FJ::D025, nullptr, &d25_y, false);
				pipeline->fill_anchor_location(FJ::D010, &d10_x, &d10_y, false);
				pipeline->fill_anchor_location(FJ::d0325, &d03_x, &d0325_y, false);
				pipeline->fill_anchor_location(FJ::d007, &d07_x, nullptr, false);

				ds->DrawLine(x + d0525_x, y + d05_y, x + d0525_x, y + d25_y,
					Colours::DimGray, default_pipeline_thickness, this->ship_style);

				ds->DrawLine(x + d03_x, y + d0325_y, x + d07_x, y + d0325_y,
					Colours::DimGray, default_pipeline_thickness, this->ship_style);

				ds->DrawLine(d10_x, y + d10_y, x + d10_x, y + d10_y,
					Colours::DimGray, default_pipeline_thickness, this->ship_style);
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
	this->gate_valve_op = make_menu<FJGVOperation, IMRMaster*>(dashboard, plc);
	this->motor_valve_op = make_menu<FJMVOperation, IMRMaster*>(dashboard, plc);
	this->upper_door_op = make_menu<FJHDOperation, IMRMaster*>(dashboard, plc);
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

		if (this->device != nullptr) {
			this->device->get_logger()->append_log_receiver(this->statusline);
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

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (mvalve != nullptr) {
		menu_popup(this->motor_valve_op, g, local_x, local_y);
	} else if (uhdoor != nullptr) {
		menu_popup(this->upper_door_op, g, local_x, local_y);
	}
}
