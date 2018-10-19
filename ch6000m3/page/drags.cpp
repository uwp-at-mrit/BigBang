#include <map>

#include "page/drags.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/dashboard/densityflowmeterlet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/device/compensatorlet.hpp"
#include "graphlet/device/overflowlet.hpp"
#include "graphlet/device/draglet.hpp"

#include "schema/di_valves.hpp"

#include "decorator/page.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum DAMode { WindowUI = 0, Dashboard };

private enum class DAOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class DA : unsigned int {
	D003, D004, D011, D012, D013, D014, D015, D016,
	LMOD, PS, SB, PSHP, SBHP,
	
	_,

	ps, sb, d13, d14,

	// dimensions
	OverflowPipe, PSCompensator, SBCompensator
};

private class Drags final : public PLCConfirmation {
public:
	Drags(DragsPage* master) : master(master) {
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->station_font = make_bold_text_format("Microsoft YaHei", tiny_font_size);

		this->drag_configs[0].trunnion_gapsize = ps_drag_trunnion_gapsize;
		this->drag_configs[0].trunnion_length = ps_drag_trunnion_length;
		this->drag_configs[0].pipe_lengths[0] = ps_drag_pipe1_length;
		this->drag_configs[0].pipe_lengths[1] = ps_drag_pipe2_length;
		this->drag_configs[0].pipe_radius = ps_drag_radius;
		this->drag_configs[0].head_width = ps_drag_head_width;
		this->drag_configs[0].head_height = ps_drag_head_length;

		this->drag_configs[1].trunnion_gapsize = sb_drag_trunnion_gapsize;
		this->drag_configs[1].trunnion_length = sb_drag_trunnion_length;
		this->drag_configs[1].pipe_lengths[0] = sb_drag_pipe1_length;
		this->drag_configs[1].pipe_lengths[1] = sb_drag_pipe2_length;
		this->drag_configs[1].pipe_radius = sb_drag_radius;
		this->drag_configs[1].head_width = sb_drag_head_width;
		this->drag_configs[1].head_height = sb_drag_head_length;
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		this->overflowpipe->set_value(RealData(DB203, 55));
		this->lengths[DA::OverflowPipe]->set_value(this->overflowpipe->get_value());

		this->set_compensator(DA::PSCompensator, DB203, 82U);
		this->set_compensator(DA::SBCompensator, DB203, 98U);

		this->set_density_speed(DA::PS, DB203, 136U);
		this->set_density_speed(DA::SB, DB203, 148U);

		this->progresses[DA::D003]->set_value(RealData(DB203, 39U), GraphletAnchor::LB);
		//this->progresses[DA::D004]->set_value(RealData(DB203, 15U), GraphletAnchor::LT);
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->overflowpipe->set_liquid_height(DBD(DB2, 224U));

		this->set_drag_positions(DA::PS, DB2, 20U);
		this->set_drag_positions(DA::SB, DB2, 108U);
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, WarGrey::SCADA::Syslog* logger) override {
		DI_gate_valve(this->valves[DA::D011], DB4, 349U, DB205, 449U);
		DI_gate_valve(this->valves[DA::D012], DB4, 333U, DB205, 457U);
		DI_gate_valve(this->valves[DA::D013], DB4, 405U, DB205, 465U);
		DI_gate_valve(this->valves[DA::D014], DB4, 373U, DB205, 473U);
		DI_gate_valve(this->valves[DA::D015], DB4, 407U, DB205, 481U);
		DI_gate_valve(this->valves[DA::D016], DB4, 375U, DB205, 489U);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load_station(float width, float height, float vinset) {
		float gridsize = vinset * 0.618F;
		float rx = gridsize;
		float ry = rx * 2.0F;
		float rsct = rx * 0.5F;
		float rlmod = gridsize * 1.5F;
		Turtle<DA>* pTurtle = new Turtle<DA>(gridsize, gridsize, DA::LMOD);

		pTurtle->jump_right(1.5F)->move_right(2.5F, DA::D011)->move_right(3, DA::sb)->move_down(4, DA::SBHP);
		pTurtle->move_right(3, DA::D003)->move_right(3, DA::SB)->jump_back();
		pTurtle->move_right(3)->move_up(4, DA::d13)->move_left(1.5F)->move_up(2, DA::D013)->jump_back();
		pTurtle->move_up(4)->move_left(1.5F)->move_up(2, DA::D015)->jump_back(DA::LMOD);

		pTurtle->jump_left(1.5F)->move_left(2.5F, DA::D012)->move_left(3, DA::ps)->move_down(4, DA::PSHP);
		pTurtle->move_left(3, DA::D004)->move_left(3, DA::PS)->jump_back();
		pTurtle->move_left(3)->move_up(4, DA::d14)->move_right(1.5F)->move_up(2, DA::D014)->jump_back();
		pTurtle->move_up(4)->move_right(1.5F)->move_up(2, DA::D016);

		this->station = this->master->insert_one(new Tracklet<DA>(pTurtle, default_pipe_thickness, default_pipe_color));
		this->load_percentage(this->progresses, DA::D003);
		this->load_percentage(this->progresses, DA::D004);
		this->load_valves(this->valves, this->labels, DA::D003, DA::D012, vinset, 0.0);
		this->load_valves(this->valves, this->labels, DA::D013, DA::D016, vinset, -90.0);
		this->load_label(this->labels, DA::LMOD.ToString(), DA::LMOD, Colours::Cyan, this->station_font);

		this->lmod = this->master->insert_one(new Arclet(0.0, 360.0, rlmod, rlmod, default_pipe_thickness, Colours::Green));
		this->hpumps[DA::PSHP] = this->master->insert_one(new Credit<HopperPumplet, DA>(+rx, -ry), DA::PSHP);
		this->hpumps[DA::SBHP] = this->master->insert_one(new Credit<HopperPumplet, DA>(-rx, -ry), DA::SBHP);
		this->suctions[DA::PS] = this->master->insert_one(new Credit<Circlelet, DA>(rsct, default_ps_color, default_pipe_thickness), DA::PS);
		this->suctions[DA::SB] = this->master->insert_one(new Credit<Circlelet, DA>(rsct, default_sb_color, default_pipe_thickness), DA::SB);
	}

	void load_dashboard(float width, float height, float vinset) {
		float shwidth, shheight, xstep, ystep;
		float cylinder_height, meter_height, drag_width;

		this->station->fill_extent(0.0F, 0.0F, &shwidth, &shheight);
		this->station->fill_stepsize(&xstep, &ystep);

		cylinder_height = shheight * 0.5F;
		meter_height = (height - vinset * 2.0F - shheight) * 0.382F;
		drag_width = width * 0.2718F;
		shwidth += (xstep * 2.0F);
		shheight += ystep;

		this->overflowpipe = this->master->insert_one(new OverflowPipelet(15.0, shheight * 0.618F));
		this->load_dimension(this->lengths, DA::OverflowPipe, "meter");

		this->load_densityflowmeters(this->dfmeters, DA::PS, DA::SB, meter_height);
		this->load_compensators(this->compensators, DA::PSCompensator, DA::SBCompensator, cylinder_height, 3.0);

		this->load_drag(this->xydrags, DA::PS, -drag_width, meter_height, this->drag_configs[0], default_ps_color);
		this->load_drag(this->xydrags, DA::SB, +drag_width, meter_height, this->drag_configs[1], default_sb_color);
	}

public:
	void reflow_station(float width, float height, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, xstep, ystep;
		float x0 = 0.0F;
		float y0 = 0.0F;
		
		this->station->fill_stepsize(&xstep, &ystep);

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case DA::D014: case DA::D016: {
				dx = x0 + xstep; dy = y0; anchor = GraphletAnchor::LC;
			}; break;
			case DA::D013: case DA::D015: {
				dx = x0 - xstep; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			default: {
				dx = x0; dy = y0 - ystep; anchor = GraphletAnchor::CB;
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->labels[it->first], anchor, dx, dy);
		}

		this->station->map_credit_graphlet(this->progresses[DA::D003], GraphletAnchor::CT, 0.0F, ystep);
		this->station->map_credit_graphlet(this->progresses[DA::D004], GraphletAnchor::CT, 0.0F, ystep);

		this->station->map_graphlet_at_anchor(this->lmod, DA::LMOD, GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->labels[DA::LMOD], GraphletAnchor::CC);
		
		this->hpumps[DA::SBHP]->fill_pump_origin(&dx, nullptr);
		this->station->map_credit_graphlet(this->hpumps[DA::SBHP], GraphletAnchor::CC, +std::fabsf(dx));
		this->station->map_credit_graphlet(this->hpumps[DA::PSHP], GraphletAnchor::CC, -std::fabsf(dx));
		this->station->map_credit_graphlet(this->suctions[DA::PS], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->suctions[DA::SB], GraphletAnchor::CC);
	}

	void reflow_dashboard(float width, float height, float vinset) {
		float gapsize = vinset * 0.5F;
		float cx = width * 0.5F;
		float xstep, ystep;

		this->station->fill_stepsize(&xstep, &ystep);

		this->master->move_to(this->lengths[DA::OverflowPipe], this->station, GraphletAnchor::CC, GraphletAnchor::CB);
		this->master->move_to(this->overflowpipe, this->lengths[DA::OverflowPipe], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);

		this->master->move_to(this->dfmeters[DA::PS], this->valves[DA::D016], GraphletAnchor::CT, GraphletAnchor::RB, 0.0F, -gapsize);
		this->master->move_to(this->dfmeters[DA::SB], this->valves[DA::D015], GraphletAnchor::RT, GraphletAnchor::LB, 0.0F, -gapsize);
		
		this->master->move_to(this->compensators[DA::PSCompensator], this->station, GraphletAnchor::LC, GraphletAnchor::RB);
		this->master->move_to(this->compensators[DA::SBCompensator], this->station, GraphletAnchor::RC, GraphletAnchor::LB);

		this->master->move_to(this->xydrags[DA::PS], xstep, height - vinset - ystep, GraphletAnchor::LB);
		this->master->move_to(this->xydrags[DA::SB], width - xstep, height - vinset - ystep, GraphletAnchor::RB);

		for (DA id = DA::PSCompensator; id <= DA::SBCompensator; id++) {
			this->master->move_to(this->lengths[id], this->compensators[id], GraphletAnchor::CB, GraphletAnchor::CT);
			this->master->move_to(this->pressures[id], this->lengths[id], GraphletAnchor::CB, GraphletAnchor::CT);
		}
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
			this->load_label(ls, id.ToString(), id, Colours::Silver, this->station_font);
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->plain_style), id);
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
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<class C, typename E>
	void load_cylinder(std::map<E, Credit<C, E>*>& cs, E id, float height, double range, Platform::String^ unit) {
		auto cylinder = new Credit<C, E>(surface, range, height * 0.2718F, height);

		cs[id] = this->master->insert_one(cylinder, id);

		this->load_dimension(this->dimensions, this->captions, id, unit);
	}

	template<class C, typename E>
	void load_densityflowmeters(std::map<E, Credit<C, E>*>& dfs, E id0, E idn, float height) {
		for (E id = id0; id <= idn; id++) {
			dfs[id] = this->master->insert_one(new Credit<C, E>(height), id);
		}
	}

	template<class C, typename E>
	void load_drag(std::map<E, Credit<C, E>*>& ds, E id, float width, float height, DragInfo& info, unsigned int visor_color) {
		ds[id] = this->master->insert_one(new Credit<C, E>(info, width, height, visor_color), id);
	}

	template<class C, typename E>
	void load_compensators(std::map<E, Credit<C, E>*>& cs, E id0, E idn, float height, double range) {
		for (E id = id0; id <= idn; id++) {
			cs[id] = this->master->insert_one(new Credit<C, E>(range, height / 2.718F, height), id);

			this->load_label(this->labels, id, Colours::Silver);
			this->lengths[id] = this->master->insert_one(new Credit<Dimensionlet, E>("meter"), id);
			this->pressures[id] = this->master->insert_one(new Credit<Dimensionlet, E>("bar"), id);
		}
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
		//this->dimensions[id]->set_value(value);
	}

	void set_compensator(DA id, const uint8* db203, unsigned int rd_idx) {
		float progress = RealData(db203, rd_idx + 2);

		this->compensators[id]->set_value(progress);
		this->lengths[id]->set_value(progress, GraphletAnchor::CC);
		this->pressures[id]->set_value(RealData(db203, rd_idx + 1), GraphletAnchor::CC);
	}

	void set_density_speed(DA id, const uint8* db203, unsigned int idx) {
		float hdensity = RealData(db203, idx + 0U);
		float udensity = RealData(db203, idx + 2U);
		float hflspeed = RealData(db203, idx + 1U);
		float uflspeed = RealData(db203, idx + 3U);

		this->dfmeters[id]->set_values(std::fmaxf(hdensity, udensity), std::fmaxf(hflspeed, uflspeed));
	}

	void set_drag_positions(DA id, const uint8* db2, unsigned int idx) {
		float3 ujoints[2];
		float3 draghead = DBD_3(db2, idx + 36U);
		float suction_depth = DBD(db2, idx + 0U);

		ujoints[0] = DBD_3(db2, idx + 12U);
		ujoints[1] = DBD_3(db2, idx + 24U);
		
		ujoints[1].y -= DBD(db2, idx + 48U);
		draghead.y -= DBD(db2, idx + 52U);
		draghead.z += DBD(db2, idx + 56U);

		this->xydrags[id]->set_position(suction_depth, ujoints, draghead);
	}

private: // never delete these graphlets manually.
	Tracklet<DA>* station;
	std::map<DA, Credit<Labellet, DA>*> labels;
	std::map<DA, Credit<GateValvelet, DA>*> valves;
	std::map<DA, Credit<HopperPumplet, DA>*> hpumps;
	std::map<DA, Credit<Circlelet, DA>*> suctions;
	std::map<DA, Credit<Cylinderlet, DA>*> cylinders;
	std::map<DA, Credit<DensitySpeedmeterlet, DA>*> dfmeters;
	std::map<DA, Credit<Compensatorlet, DA>*> compensators;
	std::map<DA, Credit<DragXZlet, DA>*> xydrags;
	std::map<DA, Credit<Dimensionlet, DA>*> pressures;
	std::map<DA, Credit<Dimensionlet, DA>*> lengths;
	std::map<DA, Credit<Dimensionlet, DA>*> rpms;
	std::map<DA, Credit<Percentagelet, DA>*> progresses;
	OverflowPipelet* overflowpipe;
	Arclet* lmod;
	
private:
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ station_font;
	DimensionStyle percentage_style;
	DimensionStyle plain_style;

private:
	DragInfo drag_configs[2];

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
			db->load_station(width, height, vinset);
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
		db->reflow_station(width, height, vinset);
		db->reflow_dashboard(width, height, vinset);
	}
}

bool DragsPage::can_select(IGraphlet* g) {
	return false;
}

void DragsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);
}
