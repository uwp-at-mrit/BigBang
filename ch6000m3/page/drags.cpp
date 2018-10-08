#include <map>

#include "page/drags.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/device/compensatorlet.hpp"
#include "graphlet/device/overflowlet.hpp"

#include "decorator/page.hpp"

#include "module.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum DAMode { WindowUI = 0, Dashboard };

private enum class HAOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class DA : unsigned int {
	D03, D04, D11, D12, D13, D14, D15, D16,
	LMOD, PS, SB, PSHP, SBHP,
	
	_,

	ps, sb, d13, d14,

	// dimensions
	OverflowPipe
};

private class Drags final : public PLCConfirmation {
public:
	Drags(DragsPage* master) : master(master) {
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->percentage_style.unit_color = Colours::Silver;
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->overflowpipe->set_value(RealData(AI_DB203, 55));
		this->dimensions[DA::OverflowPipe]->set_value(this->overflowpipe->get_value());
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->overflowpipe->set_liquid_height(DBD(DB2, 224U));
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load_pipeline(float width, float height, float vinset) {
		float gridsize = vinset;
		float rx = gridsize;
		float ry = rx * 2.0F;
		float rsct = rx * 0.5F;
		float rlmod = gridsize * 1.5F;
		Turtle<DA>* pTurtle = new Turtle<DA>(gridsize, gridsize, DA::LMOD);

		pTurtle->jump_right(1.5F)->move_right(2, DA::D11)->move_right(4, DA::sb)->move_down(4, DA::SBHP);
		pTurtle->move_right(4, DA::D03)->move_right(4, DA::SB)->jump_back();
		pTurtle->move_right(4)->move_up(4, DA::d13)->move_left(4)->move_up(2, DA::D13)->jump_back();
		pTurtle->move_up(4)->move_left(4)->move_up(2, DA::D15)->jump_back(DA::LMOD);

		pTurtle->jump_left(1.5F)->move_left(2, DA::D12)->move_left(4, DA::ps)->move_down(4, DA::PSHP);
		pTurtle->move_left(4, DA::D04)->move_left(4, DA::PS)->jump_back();
		pTurtle->move_left(4)->move_up(4, DA::d14)->move_right(4)->move_up(2, DA::D14)->jump_back();
		pTurtle->move_up(4)->move_right(4)->move_up(2, DA::D16);

		this->pipeline = this->master->insert_one(new Tracklet<DA>(pTurtle, default_pipe_thickness, default_pipe_color));
		this->load_valves(this->valves, this->labels, DA::D03, DA::D12, vinset, 0.0);
		this->load_valves(this->valves, this->labels, DA::D13, DA::D16, vinset, -90.0);
		this->load_label(this->labels, DA::LMOD.ToString(), DA::LMOD, Colours::Cyan, this->label_font);

		this->lmod = this->master->insert_one(new Arclet(0.0, 360.0, rlmod, rlmod, default_pipe_thickness, Colours::Green));
		this->hpumps[DA::PSHP] = this->master->insert_one(new Credit<HopperPumplet, DA>(+rx, -ry), DA::PSHP);
		this->hpumps[DA::SBHP] = this->master->insert_one(new Credit<HopperPumplet, DA>(-rx, -ry), DA::SBHP);
		this->suctions[DA::PS] = this->master->insert_one(new Credit<Circlelet, DA>(rsct, default_ps_color, default_pipe_thickness), DA::PS);
		this->suctions[DA::SB] = this->master->insert_one(new Credit<Circlelet, DA>(rsct, default_sb_color, default_pipe_thickness), DA::SB);
	}

	void load_dashboard(float width, float height, float vinset) {
		float overflow_size = vinset * 8.0F;

		this->overflowpipe = this->master->insert_one(new OverflowPipelet(15.0, overflow_size, overflow_size));

		this->load_dimensions(this->dimensions, DA::OverflowPipe, DA::OverflowPipe, "meter");
		//this->ps_compensator = this->master->insert_one(new Compensatorlet(3.0, 64.0F));
	}

	void reflow_pipeline(float width, float height, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin;
		float gridsize = vinset;
		float x0 = 0.0F;
		float y0 = 0.0F;
		
		this->master->move_to(this->pipeline, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case DA::D03: case DA::D04: case DA::D11: case DA::D12: {
				it->second->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
				dx = x0; dy = y0 - gridsize + margin; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
				dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RC;
			}
			}

			this->pipeline->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->pipeline->map_credit_graphlet(this->labels[it->first], anchor, dx, dy);
		}

		this->pipeline->map_graphlet_at_anchor(this->lmod, DA::LMOD, GraphletAnchor::CC);
		this->pipeline->map_credit_graphlet(this->labels[DA::LMOD], GraphletAnchor::CC);
		
		this->hpumps[DA::SBHP]->fill_pump_origin(&dx, nullptr);
		this->pipeline->map_credit_graphlet(this->hpumps[DA::SBHP], GraphletAnchor::CC, +std::fabsf(dx));
		this->pipeline->map_credit_graphlet(this->hpumps[DA::PSHP], GraphletAnchor::CC, -std::fabsf(dx));
		this->pipeline->map_credit_graphlet(this->suctions[DA::PS], GraphletAnchor::CC);
		this->pipeline->map_credit_graphlet(this->suctions[DA::SB], GraphletAnchor::CC);
	}

	void reflow_dashboard(float width, float height, float vinset) {
		this->master->move_to(this->dimensions[DA::OverflowPipe], this->pipeline, GraphletAnchor::CC, GraphletAnchor::CB);
		this->master->move_to(this->overflowpipe, this->dimensions[DA::OverflowPipe], GraphletAnchor::CT, GraphletAnchor::CB);
	}

public:
	bool on_char(VirtualKey key, IMRMaster* plc) {
		bool handled = false;

		return handled;
	}

private:
	template<class G, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, Colours::Silver, this->label_font);
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, unit, _speak(id)), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ unit) {
		this->load_label(ls, id, Colours::Silver);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, id, unit);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<typename E>
	void load_cylinder(std::map<E, Credit<Cylinderlet, E>*>& cs, E id, float height
		, double range, Platform::String^ unit, LiquidSurface surface) {
		auto cylinder = new Credit<Cylinderlet, E>(surface, range, height * 0.2718F, height);

		cs[id] = this->master->insert_one(cylinder, id);

		this->load_dimension(this->dimensions, this->captions, id, unit);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ label, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(label, font, color), id);
	}

private:
	template<class C, typename E>
	void reflow_cylinders(std::map<E, Credit<C, E>*>& is, std::map<E, Credit<Dimensionlet, E>*>& ds
		, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn) {
		float x, y, xoff, gapsize;
		float flcount = float(_I(idn) - _I(id0) + 1);
	
		this->decorator->fill_door_cell_extent(nullptr, &y, &xoff, nullptr, 1, 5.5F);
		xoff *= 0.5F;

		for (E id = id0; id <= idn; id++) {
			ls[id]->fill_extent(0.0F, 0.0F, nullptr, &gapsize);
			gapsize *= 0.5F;

			this->decorator->fill_descent_anchor(float(_I(id) - _I(id0)) / flcount, 0.0F, &x, nullptr);

			this->master->move_to(is[id], x + xoff, y, GraphletAnchor::CT);
			this->master->move_to(ls[id], is[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
			this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
		}
	}

private:
	void set_cylinder(DA id, float value) {
		this->cylinders[id]->set_value(value);
		this->dimensions[id]->set_value(value);
	}

private: // never delete these graphlets manually.
	Tracklet<DA>* pipeline;
	std::map<DA, Credit<Labellet, DA>*> labels;
	std::map<DA, Credit<Percentagelet, DA>*> progresses;
	std::map<DA, Credit<Dimensionlet, DA>*> dimensions;
	std::map<DA, Credit<Cylinderlet, DA>*> cylinders;
	std::map<DA, Credit<GateValvelet, DA>*> valves;
	std::map<DA, Credit<Compensatorlet, DA>*> compensators;
	std::map<DA, Credit<HopperPumplet, DA>*> hpumps;
	std::map<DA, Credit<Circlelet, DA>*> suctions;
	OverflowPipelet* overflowpipe;
	Arclet* lmod;
	
private:
	CanvasTextFormat^ label_font;
	DimensionStyle percentage_style;
	DimensionStyle plain_style;

private:
	DragsPage* master;
};

/*************************************************************************************************/
DragsPage::DragsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Drags* dashboard = new Drags(this);

	this->dashboard = dashboard;
	
	this->device->append_confirmation_receiver(dashboard);

	this->append_decorator(new PageDecorator());
}

DragsPage::~DragsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DragsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Drags*>(this->dashboard);

	if (db != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(DAMode::Dashboard);
			db->load_pipeline(width, height, vinset);
			db->load_dashboard(width, height, vinset);

			this->change_mode(DAMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name());
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

void DragsPage::reflow(float width, float height) {
	auto db = dynamic_cast<Drags*>(this->dashboard);
	
	if (db != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(DAMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(DAMode::Dashboard);
		db->reflow_pipeline(width, height, vinset);
		db->reflow_dashboard(width, height, vinset);
	}
}

bool DragsPage::can_select(IGraphlet* g) {
	return false;
}

void DragsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);
}
