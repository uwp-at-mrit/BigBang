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
#include "graphlet/symbol/valve/manual_valvelet.hpp"

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

	// Pumps
	PSPump, SBPump,

	// Valves
	HBV01, HBV02, HBV03, HBV08, HBV09, HBV11, HBV12, HBV13, HBV14, HBV15, HBV16, HBV17, HBV18,
	HBV04, HBV05, HBV06, HBV07, HBV10,
	SBV1, SBV2, SBV3, SBV4, SBV5,

	// Upper Hopper Doors
	SB1, SB2, SB3, SB4, SB5, SB6, SB7,
	PS1, PS2, PS3, PS4, PS5, PS6, PS7,
	
	// key labels
	PSSea, SBSea,

	_,
	// anchors used for unnamed corners
	h3ps, h3sb, h4, h5, h10,
	d0225, d0325, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for non-interconnected nodes
	nic
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
		float radius = std::fminf(gwidth, gheight);
		Turtle<FS>* pTurtle = new Turtle<FS>(gwidth, gheight, false, FS::HBV10);

		pTurtle->move_right(2, FS::SBV5)->move_right(2, FS::h10);
		
		pTurtle->turn_right_up()->move_up(2, FS::HBV08)->move_up(2)->turn_up_right(FS::h5);
		pTurtle->turn_left_up()->move_up(4)->turn_up_left();
		pTurtle->move_left(6, FS::HBV07)->move_left(6, FS::SBV4)->move_left(6, FS::Port)->jump_back(FS::h10);

		pTurtle->turn_right_down()->move_down(5, FS::HBV09)->move_down(5)->turn_down_right(FS::h4);
		pTurtle->turn_left_down()->move_down(4)->turn_down_left();
		pTurtle->move_left(6, FS::HBV06)->move_left(6, FS::SBV3)->move_left(6, FS::Starboard)->jump_back(FS::h5);

		pTurtle->move_right(5, FS::HBV05)->move_right(7.5F, FS::nic)->move_right(4.5F)->turn_right_down()->move_down(5);
		pTurtle->turn_down_left(FS::h3ps)->move_left(4, FS::PSPump)->turn_left_up();
		pTurtle->move_up(3, FS::HBV02)->move_up(2)->jump_up()->move_up(2, FS::SBV2)->move_up(2, FS::PSSea)->jump_back();

		pTurtle->turn_right_down()->move_down(2.5F, FS::HBV03)->move_down(2.5F);
		pTurtle->turn_down_left(FS::h3sb)->move_left(4, FS::SBPump)->jump_back();
		pTurtle->turn_right_down()->move_down(2)->turn_down_left()->move_left(4)->turn_left_down();
		pTurtle->move_down(FS::HBV01)->move_down(2, FS::SBV1)->move_down(2, FS::SBSea)->jump_back(FS::h4);

		pTurtle->move_right(5, FS::HBV04)->move_right(3)->turn_right_up()->move_up(2)->turn_up_right()->move_right(4);
		
		pTurtle->jump_back(FS::HBV10);
		pTurtle->move_left(28, FS::HBV18);
		
		this->pipeline = this->master->insert_one(new Tracklet<FS>(pTurtle, default_pipeline_thickness, default_pipeline_color));

		{ // load doors

			this->load_doors(this->uhdoors, this->progresses, FS::PS1, FS::PS7, radius);
			this->load_doors(this->uhdoors, this->progresses, FS::SB1, FS::SB7, radius);
		}

		{ // load valves
			this->load_valves(this->mvalves, this->vlabels, this->captions, FS::SBV1, FS::SBV2, radius, 90.0);
			this->load_valves(this->mvalves, this->vlabels, this->captions, FS::SBV3, FS::SBV5, radius, 00.0);
			this->load_valves(this->gvalves, this->vlabels, this->captions, FS::HBV01, FS::HBV18, radius, 90.0);
			this->load_valves(this->gvalves, this->vlabels, this->captions, FS::HBV04, FS::HBV10, radius, 00.0);
		}

		{ // load special nodes
			auto pscolor = Colours::make(default_port_color);
			auto sbcolor = Colours::make(default_starboard_color);
			float dh_radius = gwidth * 2.0F;
			float nic_radius = radius * 0.25F;
			
			this->load_pump(this->pumps, this->captions, FS::PSPump, -radius, +2.0F);
			this->load_pump(this->pumps, this->captions, FS::SBPump, -radius, -2.0F);
			
			this->ps_sea = this->master->insert_one(new Hatchlet(gwidth, gheight, pscolor));
			this->sb_sea = this->master->insert_one(new Hatchlet(gwidth, gheight, sbcolor));

			this->ps_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, dh_radius, gheight,
					nullptr, pscolor, default_pipeline_thickness));

			this->sb_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, dh_radius, gheight,
					nullptr, sbcolor, default_pipeline_thickness));
			
			for (FS id = FS::nic; id <= FS::nic; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipeline_thickness, default_pipeline_color));
			}
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		this->pipeline->map_graphlet_at_anchor(this->ps_draghead, FS::Port, GraphletAnchor::RC);
		this->pipeline->map_graphlet_at_anchor(this->sb_draghead, FS::Starboard, GraphletAnchor::RC);
		this->pipeline->map_graphlet_at_anchor(this->ps_sea, FS::PSSea, GraphletAnchor::CB);
		this->pipeline->map_graphlet_at_anchor(this->sb_sea, FS::SBSea, GraphletAnchor::CT);
		this->master->move_to(this->captions[FS::PSSea], this->ps_sea, GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->captions[FS::SBSea], this->sb_sea, GraphletAnchor::CB, GraphletAnchor::CT);

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
		
		{ // reflow valves
			float gridsize = std::fminf(gwidth, gheight);

			for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
				this->reflow_valve(0.0F, 0.0F, gridsize, it->first, it->second);
			}

			for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
				this->reflow_valve(0.0F, 0.0F, gridsize, it->first, it->second);
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
			this->load_label(ls, id.ToString(), id, Colours::Silver, this->label_font);
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

		this->pipeline->fill_anchor_location(FS::HBV18, &lx, &y);
		this->pipeline->fill_anchor_location(FS::HBV10, &rx, nullptr);
		cell_width = (rx - lx) / float(hopper_count);

		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;
			float x = lx + cell_width * (0.5F + float(hopper_count - idx));
			
			this->master->move_to(ds[id], x, y + yoff, GraphletAnchor::CC);
			this->master->move_to(ps[id], ds[id], d_anchor, p_anchor);
		}
	}

	template<class D, typename E>
	void reflow_valve(float x0, float y0, float gridsize, E id, D* valve) {
		GraphletAnchor anchor;
		float label_height, margin, dx, dy;

		switch (id) {
		case FS::HBV01: case FS::HBV02: case FS::HBV08: case FS::HBV09: case FS::SBV1: case FS::SBV2: {
			valve->fill_margin(x0, y0, nullptr, &margin, nullptr, nullptr);
			dx = x0 + gridsize - margin; dy = y0; anchor = GraphletAnchor::LB;
		}; break;
		case FS::HBV03: {
			valve->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
			dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RB;
		}; break;
		case FS::HBV04: case FS::HBV07: case FS::SBV4: case FS::SBV5: {
			valve->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
			dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
		}; break;
		case FS::HBV05: case FS::HBV06: case FS::HBV10: case FS::HBV18: case FS::SBV3: {
			this->vlabels[id]->fill_extent(x0, y0, nullptr, &label_height);
			valve->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
			dx = x0; dy = y0 - gridsize - label_height + margin; anchor = GraphletAnchor::CB;
		}; break;
		default: {
			this->vlabels[id]->fill_extent(x0, y0, nullptr, &label_height);
			valve->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
			dx = x0; dy = y0 - gridsize * 4.0F + margin; anchor = GraphletAnchor::CB;
		}
		}

		this->pipeline->map_credit_graphlet(valve, GraphletAnchor::CC, x0, y0);
		this->pipeline->map_credit_graphlet(this->captions[id], anchor, dx, dy);
		this->master->move_to(this->vlabels[id], this->captions[id], GraphletAnchor::CB, GraphletAnchor::CT);
	}

// never deletes these graphlets mannually
private:
	Tracklet<FS>* pipeline;
	std::map<FS, Credit<Labellet, FS>*> captions;
	std::map<FS, Credit<UpperHopperDoorlet, FS>*> uhdoors;
	std::map<FS, Credit<Percentagelet, FS>*> progresses;
	std::map<FS, Credit<HopperPumplet, FS>*> pumps;
	std::map<FS, Credit<GateValvelet, FS>*> gvalves;
	std::map<FS, Credit<ManualValvelet, FS>*> mvalves;
	std::map<FS, Credit<Labellet, FS>*> vlabels;
	std::map<FS, Omegalet*> nintercs;
	Segmentlet* ps_draghead;
	Segmentlet* sb_draghead;
	Hatchlet* ps_sea;
	Hatchlet* sb_sea;
	
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
