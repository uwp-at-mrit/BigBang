#include <map>

#include "page/drags.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/device/compensatorlet.hpp"

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
	_
};

private class Drags final : public PLCConfirmation {
public:
	Drags(DragsPage* master) : master(master) {
		this->percentage_style.unit_color = Colours::Silver;
		this->highlight_style = make_highlight_dimension_style(18.0F, 5U);
		this->setting_style = make_setting_dimension_style(18.0F, 6U);
	}

public:
	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		this->ps_compensator = this->master->insert_one(new Compensatorlet(3.0, 64.0F));
	}

	void reflow(float width, float height, float vinset) {
		this->master->move_to(this->ps_compensator, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
	}

public:
	bool on_char(VirtualKey key, IMRMaster* plc) {
		bool handled = false;

		return handled;
	}

private:
	template<typename E>
	void load_setting(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(DimensionStatus::Input, this->setting_style, unit, _speak(id)), id);
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

	template<typename E>
	void load_cylinder(std::map<E, Credit<Cylinderlet, E>*>& cs, E id, float height
		, double range, Platform::String^ unit, LiquidSurface surface) {
		auto cylinder = new Credit<Cylinderlet, E>(surface, range, height * 0.2718F, height);

		cs[id] = this->master->insert_one(cylinder, id);

		this->load_dimension(this->dimensions, this->captions, id, unit);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
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
	std::map<DA, Credit<Labellet, DA>*> captions;
	std::map<DA, Credit<Percentagelet, DA>*> progresses;
	std::map<DA, Credit<Dimensionlet, DA>*> dimensions;
	std::map<DA, Credit<Cylinderlet, DA>*> cylinders;
	std::map<DA, Credit<Dimensionlet, DA>*> ports;
	std::map<DA, Credit<Dimensionlet, DA>*> starboards;
	Compensatorlet* ps_compensator;
	Compensatorlet* sb_compensator;

private:
	DimensionStyle percentage_style;
	DimensionStyle highlight_style;
	DimensionStyle setting_style;

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
			db->load(width, height, vinset);

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
		db->reflow(width, height, vinset);
	}
}

bool DragsPage::can_select(IGraphlet* g) {
	return false;
}

void DragsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);
}
