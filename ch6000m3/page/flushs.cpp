#include <map>

#include "page/flushs.hpp"
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

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum FSMode { WindowUI = 0, Dashboard };

private enum class FSGVOperation { Open, Close, FakeOpen, FakeClose, _ };
private enum class FSHDOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class FS : unsigned int {
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

private class Flush final
	: public PLCConfirmation
	, public IMenuCommand<FSGVOperation, Credit<GateValvelet, FS>, IMRMaster*>
	, public IMenuCommand<FSHDOperation, Credit<UpperHopperDoorlet, FS>, IMRMaster*> {
public:
	Flush(FlushsPage* master) : master(master) {}

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
	void execute(FSGVOperation cmd, Credit<GateValvelet, FS>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"Gate Valve: %s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

	void execute(FSHDOperation cmd, Credit<UpperHopperDoorlet, FS>* door, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			door->id.ToString()->Data());
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
		Turtle<FS>* pTurtle = new Turtle<FS>(gwidth, gheight, false);

		pTurtle->move_left(FS::deck_rx)->move_left(2, FS::D021)->move_left(2, FS::d2122);
		pTurtle->move_down(5)->move_right(2, FS::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, FS::d1920)->move_left(2, FS::D020)->move_left(6, FS::d1720);

		pTurtle->move_left(3, FS::D017)->move_left(11, FS::n0405)->move_left(3, FS::D010)->jump_back(FS::d1720);
		
		pTurtle->move_down(3.5F, FS::PSHPump)->move_left(6, FS::n0923)->move_left(8)->move_up(1.5F, FS::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, FS::d0406)->move_right(4, FS::D006)->move_right(4)->move_down(0.5F, FS::deck_ty)->move_down(FS::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, FS::D023)->jump_back(FS::d0406);

		pTurtle->move_up(1.5F, FS::D004)->move_up(2, FS::ps)->move_up(3, FS::Port);

		pTurtle->jump_back(FS::D023)->move_down(2)->jump_down()->move_down(5, FS::D007);
		pTurtle->move_down(FS::deck_by)->move_down(0.5F, FS::d007)->jump_left(8, FS::d0325);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, FS::D025)->move_up(1.5F, FS::d0225);
		pTurtle->move_right(8, FS::n0723)->move_right(6, FS::SBHPump)->move_down(3.5F, FS::d1819)->jump_back(FS::d0225);
		pTurtle->jump_up(2.5F)->move_left(2, FS::D002)->move_left(15, FS::n24)->move_left(10, FS::D001)->move_left(3, FS::Hatch);

		pTurtle->jump_back(FS::d1819)->move_left(3, FS::D018)->move_left(11, FS::n0325)->move_left(3, FS::D008);
		pTurtle->move_left(14)->move_up(5.5F)->jump_up()->move_up(5.5F);
		pTurtle->move_up(2.5F)->turn_up_left()->move_left(3, FS::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, FS::Gantry)->move_left()->jump_back(FS::Gantry)->move_right()->jump_back(FS::d0325);

		pTurtle->move_down(1.5F, FS::D003)->move_down(2, FS::sb)->move_down(3, FS::Starboard);

		pTurtle->jump_back(FS::d1819)->move_right(4, FS::deck_lx)->move_right(2, FS::D019)->move_right(2)->move_to(FS::d1920);
		
		this->pipeline = this->master->insert_one(new Tracklet<FS>(pTurtle, default_pipeline_thickness, default_pipeline_color));

		{ // load manual pipe segement
			float d02_y, d05_y;

			this->pipeline->fill_anchor_location(FS::D002, nullptr, &d02_y);
			this->pipeline->fill_anchor_location(FS::D005, nullptr, &d05_y);

			this->manual_pipe = this->master->insert_one(
				new Linelet(0.0F, d02_y, 0.0F, d05_y,
					default_pipeline_thickness, default_pipeline_color));
		}

		{ // load doors
			float radius = std::fminf(gwidth, gheight);

			this->load_doors(this->uhdoors, this->progresses, FS::PS1, FS::PS7, radius);
			this->load_doors(this->uhdoors, this->progresses, FS::SB1, FS::SB7, radius);
		}

		{ // load valves
			float radius = std::fminf(gwidth, gheight);

			this->load_valves(this->gvalves, this->vlabels, this->captions, FS::D001, FS::D024, radius, 00.0);
			this->load_valves(this->gvalves, this->vlabels, this->captions, FS::D003, FS::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->vlabels, this->captions, FS::D004, FS::D009, radius, -90.0);
		}

		{ // load special nodes
			float radius = std::fminf(gwidth, gheight);
			float nic_radius = radius * 0.5F;
			
			this->load_pump(this->pumps, this->captions, FS::PSHPump, -radius, +2.0F);
			this->load_pump(this->pumps, this->captions, FS::SBHPump, -radius, -2.0F);
			this->ps_suction = this->master->insert_one(new Circlelet(nic_radius, default_port_color, default_pipeline_thickness));
			this->sb_suction = this->master->insert_one(new Circlelet(nic_radius, default_starboard_color, default_pipeline_thickness));
			this->sea_inlet = this->master->insert_one(new Hatchlet(radius * 2.0F));

			for (FS id = FS::n24; id <= FS::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipeline_thickness, default_pipeline_color));
			}
		}

		{ // load other labels
			this->load_label(this->captions, FS::Hatch, Colours::SeaGreen, this->caption_font);
			this->load_label(this->captions, FS::Gantry, Colours::Yellow, this->caption_font);

			for (size_t idx = 0; idx < hopper_count; idx++) {
				this->sequences[idx] = this->master->insert_one(new Labellet((idx + 1).ToString() + "#"));
				this->sequences[idx]->set_font(this->caption_font);
				this->sequences[idx]->set_color(Colours::Tomato);
			}
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin, label_height;
		float gridsize = std::fminf(gwidth, gheight);
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->manual_pipe, FS::D025, GraphletAnchor::CB);

		this->pipeline->map_credit_graphlet(this->captions[FS::Gantry], GraphletAnchor::CB);
		this->pipeline->map_graphlet_at_anchor(this->ps_suction, FS::Port, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->sb_suction, FS::Starboard, GraphletAnchor::CC);
		this->pipeline->map_graphlet_at_anchor(this->sea_inlet, FS::Hatch, GraphletAnchor::CC);
		this->master->move_to(this->captions[FS::Hatch], this->sea_inlet, GraphletAnchor::CB, GraphletAnchor::CT);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->pipeline->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipeline_thickness * 0.5F);
		}

		this->reflow_doors(this->uhdoors, this->progresses, FS::PS1, FS::PS7, gheight * -2.4F);
		this->reflow_doors(this->uhdoors, this->progresses, FS::SB1, FS::SB7, gheight * +2.4F);

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC);
			this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LC);
		}

		this->vlabels[FS::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case FS::D006: case FS::D010: case FS::D020: case FS::D021: case FS::D022: case FS::D024: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
				dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
			}; break;
			case FS::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case FS::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case FS::D001: case FS::D002: case FS::D008: case FS::D019: {
				it->second->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
				dx = x0; dy = y0 - gridsize - label_height + margin; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				it->second->fill_margin(x0, y0, nullptr, &margin, nullptr, nullptr);
				dx = x0 + gridsize - margin; dy = y0; anchor = GraphletAnchor::LB;
			}
			}

			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->pipeline->map_credit_graphlet(this->captions[it->first], anchor, dx, dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}

		{ // reflow door sequences
			this->pipeline->fill_anchor_location(FS::D008, nullptr, &y0);
			for (unsigned int idx = 0; idx < hopper_count; idx++) {
				this->master->fill_graphlet_location(this->uhdoors[_E(FS, idx + _I(FS::PS1))], &x0, nullptr, GraphletAnchor::CC);
				this->master->move_to(this->sequences[idx], x0, y0, GraphletAnchor::CB);
			}
		}
	}

public:
	void draw_relationships(CanvasDrawingSession^ ds, float Width, float Height) {
		float sx, sy, tx, ty;

		for (unsigned int idx = 0; idx < hopper_count; idx++) {
			this->master->fill_graphlet_location(this->uhdoors[_E(FS, idx + _I(FS::PS1))], &sx, &sy, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->uhdoors[_E(FS, idx + _I(FS::SB1))], &tx, &ty, GraphletAnchor::CC);
			
			ds->DrawLine(sx, sy, tx, ty, this->relationship_color, 1.0F, this->relationship_style);
		}
	}

private:
	template<class G, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, std::map<E, Credit<Labellet, E>*>& cs
		, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, "(" + id.ToString() + ")", id, Colours::Silver, this->label_font);
			this->load_label(cs, id, Colours::Silver, this->label_font);

			gs[id] = this->master->insert_one(new G(radius, degrees), id);
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
	void load_pump(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon, this->caption_font);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);
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

		this->pipeline->fill_anchor_location(FS::D001, &lx, &y);
		this->pipeline->fill_anchor_location(FS::D010, &rx, nullptr);
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
	Tracklet<FS>* pipeline;
	std::map<FS, Credit<Labellet, FS>*> captions;
	std::map<FS, Credit<UpperHopperDoorlet, FS>*> uhdoors;
	std::map<FS, Credit<Percentagelet, FS>*> progresses;
	std::map<FS, Credit<HopperPumplet, FS>*> pumps;
	std::map<FS, Credit<GateValvelet, FS>*> gvalves;
	std::map<FS, Credit<Labellet, FS>*> vlabels;
	Labellet* sequences[hopper_count];
	std::map<FS, Omegalet*> nintercs;
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
	FlushsPage* master;
};

private class FlushDecorator : public IPlanetDecorator {
public:
	FlushDecorator(Flush* master) : master(master) {
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
		auto pipeline = dynamic_cast<Tracklet<FS>*>(g);

		if (pipeline != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;

			pipeline->fill_anchor_location(FS::ps, nullptr, &ps_y, false);
			pipeline->fill_anchor_location(FS::sb, nullptr, &sb_y, false);

			pipeline->fill_anchor_location(FS::deck_lx, &deck_lx, nullptr, false);
			pipeline->fill_anchor_location(FS::deck_rx, &deck_rx, nullptr, false);
			pipeline->fill_anchor_location(FS::deck_ty, nullptr, &deck_ty, false);
			pipeline->fill_anchor_location(FS::deck_by, nullptr, &deck_by, false);

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

				pipeline->fill_anchor_location(FS::D005, &d0525_x, &d05_y, false);
				pipeline->fill_anchor_location(FS::D025, nullptr, &d25_y, false);
				pipeline->fill_anchor_location(FS::D010, &d10_x, &d10_y, false);
				pipeline->fill_anchor_location(FS::d0325, &d03_x, &d0325_y, false);
				pipeline->fill_anchor_location(FS::d007, &d07_x, nullptr, false);

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
	Flush* master;
};

FlushsPage::FlushsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Flush* dashboard = new Flush(this);

	this->dashboard = dashboard;
	this->gate_valve_op = make_menu<FSGVOperation, Credit<GateValvelet, FS>, IMRMaster*>(dashboard, plc);
	this->upper_door_op = make_menu<FSHDOperation, Credit<UpperHopperDoorlet, FS>, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new FlushDecorator(dashboard));

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

FlushsPage::~FlushsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void FlushsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Flush*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(FSMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(FSMode::WindowUI);
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

void FlushsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Flush*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(FSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(FSMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool FlushsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<GateValvelet*>(g) != nullptr)
		|| (dynamic_cast<UpperHopperDoorlet*>(g) != nullptr));
}

void FlushsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	auto uhdoor = dynamic_cast<UpperHopperDoorlet*>(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (uhdoor != nullptr) {
		menu_popup(this->upper_door_op, g, local_x, local_y);
	}
}
