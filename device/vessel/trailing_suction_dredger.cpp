#include <map>

#include "device/vessel/trailing_suction_dredger.hpp"

#include "datum/flonum.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ label_font = make_bold_text_format("Microsoft Yahei", 16.0F);

static CanvasSolidColorBrush^ label_color = Colours::DarkGray;

#define Vessel_XY(v, ref, def_x, def_y) ((v == nullptr) ? double2(def_x, def_y) : v->ref)
#define Vessel_Vertex(v, ref, xs, ys, id) v->ref = double2(xs[id]->get_input_number(), ys[id]->get_input_number())

/*************************************************************************************************/
namespace {
	// order matters
	private enum class TSD {
		GPS1, GPS2, PS_Suction, SB_Suction,
		Body1, Body2, Body3, Body4, Body5, Body6, Body7,
		Hopper1, Hopper2, Hopper3, Hopper4,
		Bridge1, Bridge2, Bridge3, Bridge4, Bridge5, Bridge6, Bridge7, Bridge8, Bridge9, Bridge10,
		Trunnion, Barge,
		_
	};
}

private class WarGrey::SCADA::TrailingSuctionDredgerSelf {
public:
	TrailingSuctionDredgerSelf(TrailingSuctionDredgerPlanet* master, TrailingSuctionDredger^ default_vessel) : master(master), label_max_width(0.0F) {
		this->input_style = make_highlight_dimension_style(label_font->FontSize, 7U, 1U);
		this->input_style.unit_color = label_color;

		this->up_to_date = true;
		this->entity = nullptr;// ref new TrailingSuctionDredger(default_vessel);
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height, float inset) {
		this->X[0] = this->master->insert_one(new Labellet("X", label_font, label_color));
		this->X[1] = this->master->insert_one(new Labellet("X", label_font, label_color));
		this->Y[0] = this->master->insert_one(new Labellet("Y", label_font, label_color));
		this->Y[1] = this->master->insert_one(new Labellet("Y", label_font, label_color));

		for (TSD id = _E0(TSD); id < TSD::_; id++) {
			double2 pt;

			switch (id) {
			case TSD::GPS1:       pt = Vessel_XY(this->entity, gps[0],     23.43, 19.88); break;
			case TSD::GPS2:       pt = Vessel_XY(this->entity, gps[1],     26.37, 13.50); break;
			case TSD::PS_Suction: pt = Vessel_XY(this->entity, ps_suction, 90.17, 0.00); break;
			case TSD::SB_Suction: pt = Vessel_XY(this->entity, sb_suction, 90.17, 24.80); break;
			case TSD::Trunnion:   pt = Vessel_XY(this->entity, trunnion,   0.00, 0.00); break;
			case TSD::Barge:      pt = Vessel_XY(this->entity, barge,      51.00, 2.20); break;

			case TSD::Body1: pt = Vessel_XY(this->entity, body_vertexes[0], 121.20, 12.40); break;
			case TSD::Body2: pt = Vessel_XY(this->entity, body_vertexes[1], 99.00, 0.00); break;
			case TSD::Body3: pt = Vessel_XY(this->entity, body_vertexes[2], 7.70, 0.00); break;
			case TSD::Body4: pt = Vessel_XY(this->entity, body_vertexes[3], 0.00, 2.60); break;
			case TSD::Body5: pt = Vessel_XY(this->entity, body_vertexes[4], 0.00, 22.20); break;
			case TSD::Body6: pt = Vessel_XY(this->entity, body_vertexes[5], 7.70, 24.80); break;
			case TSD::Body7: pt = Vessel_XY(this->entity, body_vertexes[6], 99.0, 24.80); break;

			case TSD::Hopper1: pt = Vessel_XY(this->entity, hopper_vertexes[0], 88.00, 4.20); break;
			case TSD::Hopper2: pt = Vessel_XY(this->entity, hopper_vertexes[1], 37.00, 4.20); break;
			case TSD::Hopper3: pt = Vessel_XY(this->entity, hopper_vertexes[2], 37.00, 20.60); break;
			case TSD::Hopper4: pt = Vessel_XY(this->entity, hopper_vertexes[3], 88.00, 20.60); break;

			case TSD::Bridge1:  pt = Vessel_XY(this->entity, bridge_vertexes[0], 28.00, 1.00); break;
			case TSD::Bridge2:  pt = Vessel_XY(this->entity, bridge_vertexes[1], 24.00, 1.00); break;
			case TSD::Bridge3:  pt = Vessel_XY(this->entity, bridge_vertexes[2], 24.00, 23.80); break;
			case TSD::Bridge4:  pt = Vessel_XY(this->entity, bridge_vertexes[3], 28.00, 23.80); break;
			case TSD::Bridge5:  pt = Vessel_XY(this->entity, bridge_vertexes[4], 19.00, 6.80); break;
			case TSD::Bridge6:  pt = Vessel_XY(this->entity, bridge_vertexes[5], 19.00, 18.00); break;
			case TSD::Bridge7:  pt = Vessel_XY(this->entity, bridge_vertexes[6], 29.00, 17.80); break;
			case TSD::Bridge8:  pt = Vessel_XY(this->entity, bridge_vertexes[7], 32.00, 14.00); break;
			case TSD::Bridge9:  pt = Vessel_XY(this->entity, bridge_vertexes[8], 32.00, 10.80); break;
			case TSD::Bridge10: pt = Vessel_XY(this->entity, bridge_vertexes[9], 29.00, 7.00); break;
			}

			this->labels[id] = this->insert_label(id);
			this->xs[id] = this->insert_input_field(id, pt.x);
			this->ys[id] = this->insert_input_field(id, pt.y);
		}

		this->dredger = this->master->insert_one(new TrailingSuctionDredgerlet("vessel"));
	}

	void reflow(IGraphlet* frame, float width, float height, float inset) {
		float xoff = inset * 2.0F + this->label_max_width;
		float pwidth, pheight;

		this->xs[TSD::GPS1]->fill_extent(0.0F, 0.0F, &pwidth, &pheight);
		this->master->move_to(this->X[0], frame, GraphletAnchor::LT, GraphletAnchor::CT, xoff + pwidth * 0.5F, inset);
		this->master->move_to(this->Y[0], this->X[0], GraphletAnchor::RC, GraphletAnchor::LC, pwidth);
		this->master->move_to(this->X[1], this->Y[0], GraphletAnchor::RC, GraphletAnchor::LC, xoff + pwidth);
		this->master->move_to(this->Y[1], this->X[1], GraphletAnchor::RC, GraphletAnchor::LC, pwidth);

		this->reflow_input_fields(this->X[0], _E0(TSD), TSD::Bridge1, inset, pheight, TSD::Hopper1);
		this->reflow_input_fields(this->X[1], TSD::Bridge1, TSD::_, inset, pheight, TSD::Trunnion);

		this->master->move_to(this->dredger, frame, GraphletAnchor::RT, GraphletAnchor::RT, -inset, inset);
	}

public:
	void on_edit(Credit<Dimensionlet, TSD>* dim) {
		this->up_to_date = false;
		this->refresh_entity();

		this->dredger->moor(GraphletAnchor::RB);
		this->dredger->preview(this->entity);
		this->dredger->notify_updated();
	}

	void on_apply() {
		this->refresh_entity();
		this->dredger->refresh(this->entity);
	}

public:
	IGraphlet* thumbnail() {
		return this->dredger;
	}

private:
	void refresh_entity() {
		if (this->entity == nullptr) {
			this->entity = ref new TrailingSuctionDredger();
		}

		if (!this->up_to_date) {
			this->up_to_date = true;

			Vessel_Vertex(this->entity, gps[0], this->xs, this->ys, TSD::GPS1);
			Vessel_Vertex(this->entity, gps[1], this->xs, this->ys, TSD::GPS2);
			Vessel_Vertex(this->entity, ps_suction, this->xs, this->ys, TSD::PS_Suction);
			Vessel_Vertex(this->entity, sb_suction, this->xs, this->ys, TSD::SB_Suction);
			Vessel_Vertex(this->entity, trunnion, this->xs, this->ys, TSD::Trunnion);
			Vessel_Vertex(this->entity, barge, this->xs, this->ys, TSD::Barge);

			Vessel_Vertex(this->entity, body_vertexes[0], this->xs, this->ys, TSD::Body1);
			Vessel_Vertex(this->entity, body_vertexes[1], this->xs, this->ys, TSD::Body2);
			Vessel_Vertex(this->entity, body_vertexes[2], this->xs, this->ys, TSD::Body3);
			Vessel_Vertex(this->entity, body_vertexes[3], this->xs, this->ys, TSD::Body4);
			Vessel_Vertex(this->entity, body_vertexes[4], this->xs, this->ys, TSD::Body5);
			Vessel_Vertex(this->entity, body_vertexes[5], this->xs, this->ys, TSD::Body6);
			Vessel_Vertex(this->entity, body_vertexes[6], this->xs, this->ys, TSD::Body7);

			Vessel_Vertex(this->entity, hopper_vertexes[0], this->xs, this->ys, TSD::Hopper1);
			Vessel_Vertex(this->entity, hopper_vertexes[1], this->xs, this->ys, TSD::Hopper2);
			Vessel_Vertex(this->entity, hopper_vertexes[2], this->xs, this->ys, TSD::Hopper3);
			Vessel_Vertex(this->entity, hopper_vertexes[3], this->xs, this->ys, TSD::Hopper4);

			Vessel_Vertex(this->entity, bridge_vertexes[0], this->xs, this->ys, TSD::Bridge1);
			Vessel_Vertex(this->entity, bridge_vertexes[1], this->xs, this->ys, TSD::Bridge2);
			Vessel_Vertex(this->entity, bridge_vertexes[2], this->xs, this->ys, TSD::Bridge3);
			Vessel_Vertex(this->entity, bridge_vertexes[3], this->xs, this->ys, TSD::Bridge4);
			Vessel_Vertex(this->entity, bridge_vertexes[4], this->xs, this->ys, TSD::Bridge5);
			Vessel_Vertex(this->entity, bridge_vertexes[5], this->xs, this->ys, TSD::Bridge6);
			Vessel_Vertex(this->entity, bridge_vertexes[6], this->xs, this->ys, TSD::Bridge7);
			Vessel_Vertex(this->entity, bridge_vertexes[7], this->xs, this->ys, TSD::Bridge8);
			Vessel_Vertex(this->entity, bridge_vertexes[8], this->xs, this->ys, TSD::Bridge9);
			Vessel_Vertex(this->entity, bridge_vertexes[9], this->xs, this->ys, TSD::Bridge10);
		}
	}

private:
	Labellet* insert_label(TSD id) {
		Labellet* label = new Labellet(_speak(id), label_font, label_color);
		float lbl_width;

		label->fill_extent(0.0F, 0.0F, &lbl_width, nullptr);
		this->label_max_width = flmax(this->label_max_width, lbl_width);

		return this->master->insert_one(label);
	}
	
	Credit<Dimensionlet, TSD>* insert_input_field(TSD id, double v) {
		auto input = new Credit<Dimensionlet, TSD>(DimensionState::Input, this->input_style, "meter");

		this->master->insert_one(input, id);
		input->set_value(v);

		return input;
	}

	void reflow_input_fields(IGraphlet* col, TSD id0, TSD idp1, float gapsize, float pheight, TSD sep) {
		float idx0 = _F(id0);
		float yoff0 = gapsize * 0.5F;
		float hgap = gapsize * 0.618F;

		for (TSD id = id0; id < idp1; id++) {
			float yrow = (pheight + gapsize) * (_F(id) - idx0);

			if (id == sep) { // add a blank before `sep`
				yoff0 = gapsize;
			}

			this->master->move_to(this->xs[id], col, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, yoff0 + yrow);
			this->master->move_to(this->ys[id], this->xs[id], GraphletAnchor::RC, GraphletAnchor::LC, hgap);
			this->master->move_to(this->labels[id], this->xs[id], GraphletAnchor::LC, GraphletAnchor::RC, -hgap);
		}
	}

private:
	float label_max_width;
	DimensionStyle input_style;
	TrailingSuctionDredger^ entity;
	bool up_to_date;

private: // never delete these graphlet manually
	WarGrey::SCADA::TrailingSuctionDredgerlet* dredger;
	WarGrey::SCADA::Labellet* X[2];
	WarGrey::SCADA::Labellet* Y[2];
	std::map<TSD, Labellet*> labels;
	std::map<TSD, Credit<Dimensionlet, TSD>*> xs;
	std::map<TSD, Credit<Dimensionlet, TSD>*> ys;

private:
	TrailingSuctionDredgerPlanet* master;
};

/*************************************************************************************************/
TrailingSuctionDredgerPlanet::TrailingSuctionDredgerPlanet(TrailingSuctionDredger^ default_vessel) : EditorPlanet(__MODULE__) {
	this->self = new TrailingSuctionDredgerSelf(this, default_vessel);
}

TrailingSuctionDredgerPlanet::~TrailingSuctionDredgerPlanet() {
	delete this->self;
}

void TrailingSuctionDredgerPlanet::load(CanvasCreateResourcesReason reason, float width, float height) {
	float bg_width, bg_height;

	EditorPlanet::load(reason, width, height);

	this->background->fill_extent(0.0F, 0.0F, &bg_width, &bg_height);
	this->self->load(reason, bg_width, bg_height, (width - bg_width) * 0.5F);
}

void TrailingSuctionDredgerPlanet::reflow(float width, float height) {
	float bg_width, bg_height;
	
	EditorPlanet::reflow(width, height);

	this->background->fill_extent(0.0F, 0.0F, &bg_width, &bg_height);
	this->self->reflow(this->background, width, height, (width - bg_width) * 0.5F);
}

IGraphlet* TrailingSuctionDredgerPlanet::thumbnail_graphlet() {
	return this->self->thumbnail();
}

void TrailingSuctionDredgerPlanet::on_apply() {
	this->self->on_apply();
}

void TrailingSuctionDredgerPlanet::on_edit(Dimensionlet* dim) {
	this->self->on_edit(static_cast<Credit<Dimensionlet, TSD>*>(dim));
}
