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
#include "graphlet/symbol/valve/manual_valvelet.hpp"
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

private enum class LDOperation { Open, Close, FakeOpen, FakeClose, _ };
private enum class LDMVOperation { Open, Close, FakeOpen, FakeClose, Heat, _ };

static ICanvasBrush^ block_color = Colours::Firebrick;
static ICanvasBrush^ nonblock_color = Colours::WhiteSmoke;

// WARNING: order matters
private enum class LD : unsigned int {
	Port, Starboard,
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024, D026,
	D003, D007, D011, D023, D025,
	D004, D005, D009, D012,
	D014, D016,
	D013, D015,
	// Key Labels
	PSUWPump, SBUWPump, PSHPump, SBHPump, Gantry, LMOD,
	_,
	// anchors used as last jumping points
	d11, d12, d13, d14, d24,
	d0225, d0326, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, gantry, barge, deck_lx, deck_rx, deck_ty, deck_by,

	// anchors used for non-interconnected nodes
	n0325, n0405, n0723, n0923
};

private class Barge final
	: public PLCConfirmation
	, public IMenuCommand<LDOperation, IMRMaster*>
	, public IMenuCommand<LDMVOperation, IMRMaster*> {
public:
	Barge(LoadsPage* master) : master(master) {}

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
	void execute(LDOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto valve = dynamic_cast<Credit<ManualValvelet, LD>*>(target);

		if (valve != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"Valve: %s %s",
				cmd.ToString()->Data(),
				valve->id.ToString()->Data());
		}
	}

	void execute(LDMVOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto valve = dynamic_cast<Credit<TValvelet, LD>*>(target);

		if (valve != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"MValve: %s %s",
				cmd.ToString()->Data(),
				valve->id.ToString()->Data());
		}
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", 14.0F);
		this->label_font = make_bold_text_format("Microsoft YaHei", 10.0F);
		this->dimension_style = make_highlight_dimension_style(gheight, 5U);
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<LD>* pTurtle = new Turtle<LD>(gwidth, gheight, false);

		pTurtle->move_left(LD::deck_rx)->move_left(2, LD::D021)->move_left(2, LD::d2122);
		pTurtle->move_down(5)->move_right(2, LD::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, LD::d1920)->move_left(2, LD::D020)->move_left(6, LD::d1720);

		pTurtle->move_left(3, LD::D017)->move_left(11, LD::n0405)->move_left(4, LD::D010)->move_left(6, LD::d12);
		pTurtle->move_down(1.5F, LD::D012)->move_down(3.5F)->jump_down(LD::LMOD)->jump_back(LD::d12);
		pTurtle->move_left(4, LD::d14)->move_left_down(1.5F, LD::D014)->move_left_down(1.5F)->jump_back();
		pTurtle->move_left(5, LD::d24)->move_left(5)->move_left_down(1.5F, LD::D016)->move_left_down(1.5F)->jump_back();
		pTurtle->jump_up(3, LD::gantry)->move_left(4, LD::D024)->move_left(4)->move_up(LD::Gantry);
		pTurtle->move_left()->jump_back(LD::Gantry)->move_right()->jump_back(LD::d1720);
		
		pTurtle->move_down(3.5F, LD::PSHPump)->move_left(6, LD::n0923)->move_left(8)->move_up(1.5F, LD::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, LD::d0406)->move_right(4, LD::D006)->move_right(4)->move_down(0.5F, LD::deck_ty)->move_down(LD::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, LD::D023)->jump_back(LD::d0406);

		pTurtle->move_up(1.5F, LD::D004)->move_up(2, LD::ps)->move_up(2)->turn_up_left();
		pTurtle->move_left(10, LD::PSUWPump)->move_left(10, LD::Port);

		pTurtle->jump_back(LD::D023)->move_down(2)->jump_down()->move_down(5, LD::D007);
		pTurtle->move_down(LD::deck_by)->move_down(0.5F)->move_left(4, LD::D026)->move_left(4, LD::d0326);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, LD::D025)->move_up(1.5F, LD::d0225);
		pTurtle->move_right(8, LD::n0723)->move_right(6, LD::SBHPump)->move_down(3.5F, LD::d1819)->jump_back(LD::d0225);
		pTurtle->move_up(2.5F)->move_left(2, LD::D002)->move_left(28, LD::D001)->move_left(2)->jump_back(LD::d1819);

		pTurtle->move_left(3, LD::D018)->move_left(11, LD::n0325)->move_left(4, LD::D008)->move_left(6, LD::d11);
		pTurtle->move_up(1.5F, LD::D011)->move_up(3.5F)->jump_back();
		pTurtle->move_left(4, LD::d13)->move_left_up(1.5F, LD::D013)->move_left_up(1.5F)->jump_back();
		pTurtle->move_left(5, LD::barge)->move_left(5)->move_left_up(1.5F, LD::D015)->move_left_up(1.5F)->jump_back(LD::d0326);

		pTurtle->move_down(1.5F, LD::D003)->move_down(2, LD::sb)->move_down(2)->turn_down_left();
		pTurtle->move_left(10, LD::SBUWPump)->move_left(10, LD::Starboard);

		pTurtle->jump_back(LD::d1819)->move_right(4, LD::deck_lx)->move_right(2, LD::D019)->move_right(2)->move_to(LD::d1920);
		
		this->pipeline = this->master->insert_one(new Tracklet<LD>(pTurtle, default_pipeline_thickness, default_pipeline_color));

		{ // load valves
			float radius = std::fminf(gwidth, gheight);

			this->load_valve(this->valves, this->vlabels, this->captions, LD::D001, radius, -90.0);
			this->load_valves(this->valves, this->mvalves, this->vlabels, this->captions, LD::D002, LD::D026, radius, -90.0, 90.0);
			this->load_valves(this->valves, this->mvalves, this->vlabels, this->captions, LD::D003, LD::D025, radius, 0.0, 90.0);
			this->load_valves(this->valves, this->mvalves, this->vlabels, this->captions, LD::D004, LD::D012, radius, 180.0, 90.0);
			this->load_valves(this->valves, this->mvalves, this->vlabels, this->captions, LD::D014, LD::D016, radius, 225.0, 90.0);
			this->load_valves(this->valves, this->mvalves, this->vlabels, this->captions, LD::D013, LD::D015, radius, -45.0, 90.0);
		}

		{ // load special nodes
			float radius = std::fminf(gwidth, gheight);
			float nic_radius = radius * 0.5F;
			float barge_ty, barge_by;

			this->pipeline->fill_anchor_location(LD::gantry, nullptr, &barge_ty, false);
			this->pipeline->fill_anchor_location(LD::barge, nullptr, &barge_by, false);

			this->gantry_line = this->master->insert_one(
				new Linelet(0.0F, 0.0F, 0.0F, std::fabsf(barge_by - barge_ty) - default_pipeline_thickness,
					default_pipeline_thickness * 1.618F, default_pipeline_color,
					make_dash_stroke(CanvasDashStyle::Dash)));

			this->LMOD = this->master->insert_one(new Arclet(0.0, 360.0, radius, radius, 0.5F, default_pipeline_color));

			this->ps_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_port_color, default_pipeline_thickness));

			this->sb_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_starboard_color, default_pipeline_thickness));

			for (LD id = LD::n0325; id <= LD::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Arclet(-92.0, 92.0, nic_radius, nic_radius,
						default_pipeline_thickness, default_pipeline_color));
			}
		}

		{ // load labels
			this->load_labels(this->captions, LD::PSUWPump, LD::SBHPump, Colours::Salmon, this->caption_font);
			this->load_label(this->captions, LD::Gantry, Colours::Yellow, this->caption_font);
			this->load_label(this->captions, LD::LMOD, Colours::Yellow, this->label_font);
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy;
		float valve_adjust_gridsize = std::fminf(gwidth, gheight) * 0.618F;
		float label_height;
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		this->pipeline->map_credit_graphlet(this->captions[LD::PSUWPump], GraphletAnchor::CB);
		this->pipeline->map_credit_graphlet(this->captions[LD::SBUWPump], GraphletAnchor::CT);
		this->pipeline->map_credit_graphlet(this->captions[LD::PSHPump], GraphletAnchor::LC);
		this->pipeline->map_credit_graphlet(this->captions[LD::SBHPump], GraphletAnchor::LC);
		this->pipeline->map_credit_graphlet(this->captions[LD::Gantry], GraphletAnchor::CB);
		this->pipeline->map_credit_graphlet(this->captions[LD::LMOD], GraphletAnchor::CB);

		this->pipeline->map_graphlet_at_anchor(this->ps_draghead, LD::Port, GraphletAnchor::RC);
		this->pipeline->map_graphlet_at_anchor(this->sb_draghead, LD::Starboard, GraphletAnchor::RC);
		this->pipeline->map_graphlet_at_anchor(this->gantry_line, LD::gantry, GraphletAnchor::CT);
		this->pipeline->map_graphlet_at_anchor(this->LMOD, LD::LMOD, GraphletAnchor::CC);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			this->pipeline->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC);
		}

		this->vlabels[LD::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case LD::D014: case LD::D016: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D013: case LD::D015: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D006: case LD::D010: case LD::D017: case LD::D020: case LD::D021: case LD::D022: case LD::D024: {
				dx = x0; dy = y0 + valve_adjust_gridsize; anchor = GraphletAnchor::CT;
			}; break;
			default: {
				if (it->second->get_direction_degrees() == -90.0) {
					dx = x0; dy = y0 - valve_adjust_gridsize - label_height; anchor = GraphletAnchor::CB;
				} else {
					dx = x0 + valve_adjust_gridsize; dy = y0; anchor = GraphletAnchor::LB;
				}
			}
			}

			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->pipeline->map_credit_graphlet(this->captions[it->first], anchor, dx, dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case LD::D014: case LD::D016: {
				dx = x0 - valve_adjust_gridsize; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			case LD::D013: case LD::D015: {
				dx = x0 - valve_adjust_gridsize; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			case LD::D006: case LD::D010: case LD::D017: case LD::D020: case LD::D021: case LD::D022: case LD::D024: {
				dx = x0; dy = y0 - valve_adjust_gridsize; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				if (it->second->get_direction_degrees() == -90.0) {
					dx = x0; dy = y0 + valve_adjust_gridsize; anchor = GraphletAnchor::CT;
				} else {
					dx = x0 - valve_adjust_gridsize; dy = y0; anchor = GraphletAnchor::CB;
				}
			}
			}

			this->pipeline->map_credit_graphlet(this->mvalves[it->first], anchor, dx, dy);
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
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees, double mdelta) {
		float mradius = radius * 0.75F;
		double mdegrees = degrees + mdelta;

		for (E id = id0; id <= idn; id++) {
			ms[id] = this->master->insert_one(new M(mradius, mdegrees), id);
			this->load_valve(gs, ls, cs, id, radius, degrees);
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

	template<typename E>
	void load_labels(std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, color, font);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<LD>* pipeline;
	std::map<LD, Credit<Labellet, LD>*> captions;
	std::map<LD, Credit<ManualValvelet, LD>*> valves;
	std::map<LD, Credit<TValvelet, LD>*> mvalves;
	std::map<LD, Credit<Labellet, LD>*> vlabels;
	std::map<LD, Arclet*> nintercs;
	Segmentlet* ps_draghead;
	Segmentlet* sb_draghead;
	Linelet* gantry_line;
	Arclet* LMOD;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;

private:
	LoadsPage* master;
};


private class ShipDecorator : public IPlanetDecorator {
public:
	ShipDecorator() {
		float height = 1.0F;
		float xradius = height * 0.10F;
		float yradius = height * 0.50F;

		this->ship_width = 1.0F - xradius;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, yradius, -90.0, 90.0, xradius, yradius));

		this->ship_style = make_dash_stroke(CanvasDashStyle::Dash);
	}

public:
	void draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool is_selected) override {
		auto pipeline = dynamic_cast<Tracklet<LD>*>(g);

		if (pipeline != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;

			pipeline->fill_anchor_location(LD::ps, nullptr, &ps_y, false);
			pipeline->fill_anchor_location(LD::sb, nullptr, &sb_y, false);

			pipeline->fill_anchor_location(LD::deck_lx, &deck_lx, nullptr, false);
			pipeline->fill_anchor_location(LD::deck_rx, &deck_rx, nullptr, false);
			pipeline->fill_anchor_location(LD::deck_ty, nullptr, &deck_ty, false);
			pipeline->fill_anchor_location(LD::deck_by, nullptr, &deck_by, false);

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
		}
	}

private:
	CanvasGeometry^ ship;
	CanvasStrokeStyle^ ship_style;

private:
	float ship_width;
};

LoadsPage::LoadsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Barge* dashboard = new Barge(this);

	this->dashboard = dashboard;
	this->manual_valve_op = make_menu<LDOperation, IMRMaster*>(dashboard, plc);
	this->motor_valve_op = make_menu<LDMVOperation, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new ShipDecorator());

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

		if (this->device != nullptr) {
			this->device->get_logger()->append_log_receiver(this->statusline);
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
	return ((dynamic_cast<ManualValvelet*>(g) != nullptr) || (dynamic_cast<TValvelet*>(g) != nullptr));
}

void LoadsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto valve = dynamic_cast<ManualValvelet*>(g);
	auto mvalve = dynamic_cast<TValvelet*>(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (valve != nullptr) {
		menu_popup(this->manual_valve_op, g, local_x, local_y);
	} else if (mvalve != nullptr) {
		menu_popup(this->motor_valve_op, g, local_x, local_y);
	}
}
