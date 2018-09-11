#include <map>

#include "page/barges.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/symbol/pumplet.hpp"
#include "graphlet/symbol/valvelet.hpp"
#include "graphlet/dashboard/thermometerlet.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum LBMode { WindowUI = 0, Dashboard };

private enum class LBOperation { Open, Close, FakeOpen, FakeClose, _ };

static ICanvasBrush^ block_color = Colours::Firebrick;
static ICanvasBrush^ nonblock_color = Colours::WhiteSmoke;

// WARNING: order matters
private enum class LB : unsigned int {
	Port, Starboard,
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024, D026,
	D003, D007, D011, D023, D025,
	D004, D005, D009, D012,
	D014, D016,
	D013, D015,
	// Key Labels
	PSUWPump, SBUWPump, PSHPump, SBHPump, Bracket,
	_,
	// anchors used as last jumping points
	d11, d12, d13, d14, d24,
	d0225, d0326, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, deck_lx, deck_rx, deck_ty, deck_by
};

private class BargeDecorator : public IPlanetDecorator {
public:
	BargeDecorator() {
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
		auto pipelines = dynamic_cast<Tracklet<LB>*>(g);

		if (pipelines != nullptr) {
			float ps_y, sb_y, deck_lx, deck_ty, deck_rx, deck_by;

			pipelines->fill_anchor_location(LB::ps, nullptr, &ps_y, false);
			pipelines->fill_anchor_location(LB::sb, nullptr, &sb_y, false);

			pipelines->fill_anchor_location(LB::deck_lx, &deck_lx, nullptr, false);
			pipelines->fill_anchor_location(LB::deck_rx, &deck_rx, nullptr, false);
			pipelines->fill_anchor_location(LB::deck_ty, nullptr, &deck_ty, false);
			pipelines->fill_anchor_location(LB::deck_by, nullptr, &deck_by, false);

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


private class Barge final : public PLCConfirmation, public IMenuCommand<LBOperation, IMRMaster*> {
public:
	Barge(BargePage* master) : master(master) {}

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
	void execute(LBOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto valve = dynamic_cast<Credit<Valvelet, LB>*>(target);

		if (valve != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				valve->id.ToString()->Data());
		}
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_text_format("Microsoft YaHei", 14.0F);
		this->dimension_style = make_highlight_dimension_style(gheight, 5U);
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<LB>* pTurtle = new Turtle<LB>(gwidth, gheight, false);

		pTurtle->move_left(LB::deck_rx)->move_left(2, LB::D021)->move_left(2, LB::d2122);
		pTurtle->move_down(3)->move_right(2, LB::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, LB::d1920)->move_left(2, LB::D020)->move_left(6, LB::d1720);

		pTurtle->move_left(3, LB::D017)->move_left(10.5F)->turn_up_left_down()->move_left(3, LB::D010)->move_left(6, LB::d12);
		pTurtle->move_down(1.5F, LB::D012)->move_down(4)->turn_right_down_left()->jump_back();
		pTurtle->move_left(4, LB::d14)->move_left_down(1.5F, LB::D014)->move_left_down(1.5F)->jump_back();
		pTurtle->move_left(4, LB::d24)->move_left(6)->move_left_down(1.5F, LB::D016)->move_left_down(1.5F)->jump_back();
		pTurtle->move_up(2)->move_left(4, LB::D024)->move_left(6, LB::Bracket)->jump_back(LB::d1720);
		
		pTurtle->move_down(3.5F, LB::PSHPump)->move_left(14)->move_up(2, LB::D005);
		pTurtle->move_up(5, LB::d0406)->move_right(4, LB::D006)->move_right(4)->move_down(0.5F, LB::deck_ty)->move_down(LB::D009);
		pTurtle->move_down(5)->turn_right_down_left()->move_down(2, LB::D023)->jump_back(LB::d0406);

		pTurtle->move_up(1.5F, LB::D004)->move_up(LB::ps)->move_up(2)->turn_up_left();
		pTurtle->move_left(12, LB::PSUWPump)->move_left(12, LB::Port);

		pTurtle->jump_back(LB::D023)->move_down(2)->turn_right_down_left()->move_down(4.5F, LB::D007);
		pTurtle->move_down(LB::deck_by)->move_down(0.5F)->move_left(4, LB::D026)->move_left(4, LB::d0326)->move_up(5, LB::D025);
		pTurtle->move_up(1.5F, LB::d0225)->move_right(14, LB::SBHPump)->move_down(3, LB::d1819)->jump_back(LB::d0225);
		pTurtle->move_up(2.5F)->move_left(2, LB::D002)->move_left(28, LB::D001)->move_left(2)->jump_back(LB::d1819);

		pTurtle->move_left(3, LB::D018)->move_left(10.5F)->turn_down_left_up()->move_left(3, LB::D008)->move_left(6, LB::d11);
		pTurtle->move_up(1.5F, LB::D011)->move_up(3.5F)->turn_left_up_right()->jump_back();
		pTurtle->move_left(4, LB::d13)->move_left_up(1.5F, LB::D013)->move_left_up(1.5F)->jump_back();
		pTurtle->move_left(4)->move_left(6)->move_left_up(1.5F, LB::D015)->move_left_up(1.5F)->jump_back(LB::d0326);

		pTurtle->move_down(1.5F, LB::D003)->move_down(LB::sb)->move_down(3)->turn_down_left();
		pTurtle->move_left(12, LB::SBUWPump)->move_left(12, LB::Starboard);

		pTurtle->jump_back(LB::d1819)->move_right(4, LB::deck_lx)->move_right(2, LB::D019)->move_right(2)->move_to(LB::d1920);
		
		this->pipeline = this->master->insert_one(new Tracklet<LB>(pTurtle, default_pipeline_thickness, default_pipeline_color));

		this->load_labels(this->captions, LB::PSUWPump, LB::SBHPump, Colours::Salmon);
		this->load_label(this->captions, LB::Bracket, Colours::Yellow);

		{ // load valves
			float radius = std::fminf(gwidth, gheight);

			this->load_valves(this->valves, this->vlabels, this->captions, LB::D001, LB::D026, radius, -90.0);
			this->load_valves(this->valves, this->vlabels, this->captions, LB::D003, LB::D025, radius, 0.0);
			this->load_valves(this->valves, this->vlabels, this->captions, LB::D004, LB::D012, radius, 180.0);
			this->load_valves(this->valves, this->vlabels, this->captions, LB::D014, LB::D016, radius, 225.0);
			this->load_valves(this->valves, this->vlabels, this->captions, LB::D013, LB::D015, radius, -45.0);
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor lbl_a;
		float lbl_dx, lbl_dy;
		float valve_adjust_gridsize = gheight * 0.618F;
		float label_height;
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		this->pipeline->map_credit_graphlet(this->captions[LB::PSUWPump], GraphletAnchor::CB);
		this->pipeline->map_credit_graphlet(this->captions[LB::SBUWPump], GraphletAnchor::CT);
		this->pipeline->map_credit_graphlet(this->captions[LB::PSHPump], GraphletAnchor::LC);
		this->pipeline->map_credit_graphlet(this->captions[LB::SBHPump], GraphletAnchor::LC);
		this->pipeline->map_credit_graphlet(this->captions[LB::Bracket], GraphletAnchor::CC);

		this->vlabels[LB::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case LB::D014: case LB::D016: {
				lbl_dx = x0 + gwidth; lbl_dy = y0; lbl_a = GraphletAnchor::LB;
			}; break;
			case LB::D013: case LB::D015: {
				lbl_dx = x0 + gwidth; lbl_dy = y0; lbl_a = GraphletAnchor::LB;
			}; break;
			case LB::D006: case LB::D008: case LB::D017: case LB::D019: case LB::D022: {
				lbl_dx = x0; lbl_dy = y0 + valve_adjust_gridsize; lbl_a = GraphletAnchor::CT;
			}; break;
			default: {
				if (it->second->get_direction_degrees() == -90.0) {
					lbl_dx = x0; lbl_dy = y0 - valve_adjust_gridsize - label_height; lbl_a = GraphletAnchor::CB;
				} else {
					lbl_dx = x0 + valve_adjust_gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LB;
				}
			}
			}

			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->pipeline->map_credit_graphlet(this->captions[it->first], lbl_a, lbl_dx, lbl_dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}
	}

private:
	template<class G, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
			
			this->load_label(ls, "(" + id.ToString() + ")", id, Colours::Silver);
			this->load_label(cs, id, Colours::Silver);
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
	void load_labels(std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, CanvasSolidColorBrush^ color) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, color);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<LB>* pipeline;
	std::map<LB, Credit<Labellet, LB>*> captions;
	std::map<LB, Credit<Valvelet, LB>*> valves;
	std::map<LB, Credit<Labellet, LB>*> vlabels;
	
private:
	CanvasTextFormat^ caption_font;
	DimensionStyle dimension_style;

private:
	BargePage* master;
};

BargePage::BargePage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Barge* dashboard = new Barge(this);

	this->dashboard = dashboard;
	this->operation = make_menu<LBOperation, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new BargeDecorator());

#ifdef _DEBUG
		this->append_decorator(this->grid);
#endif
	}
}

BargePage::~BargePage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void BargePage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Barge*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(LBMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(LBMode::WindowUI);
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

void BargePage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Barge*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(LBMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(LBMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool BargePage::can_select(IGraphlet* g) {
	return (dynamic_cast<Valvelet*>(g) != nullptr);
}

void BargePage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
