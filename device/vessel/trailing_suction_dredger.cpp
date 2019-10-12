#include <map>

#include "device/vessel/trailing_suction_dredger.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/planetlet.hpp"

#include "datum/flonum.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ label_font = make_bold_text_format("Microsoft Yahei", 16.0F);
static CanvasTextFormat^ sketch_number_font = make_bold_text_format("Consolas", 14.0F);
static CanvasTextFormat^ sketch_font = make_bold_text_format("Consolas", 8.0F);

static CanvasSolidColorBrush^ label_color = Colours::DarkGray;
static CanvasSolidColorBrush^ sketch_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ axes_color = Colours::Salmon;
static CanvasSolidColorBrush^ hopper_color = Colours::Khaki;
static CanvasSolidColorBrush^ bridge_color = Colours::RoyalBlue;

#define Vessel_Display_Vertex(v, ref, xs, ys, id) xs[id]->set_value(v->ref.x); ys[id]->set_value(v->ref.y)
#define Vessel_Refresh_Vertex(v, ref, xs, ys, id) v->ref = double2(xs[id]->get_value(), ys[id]->get_value())

/*************************************************************************************************/
namespace {
	// order matters
	private enum class TSD {
		GPS1, GPS2, PS_Suction, SB_Suction,
		Body1, Body2, Body3, Body4, Body5, Body6, Body7,
		Hopper1, Hopper2, Hopper3, Hopper4,
		Bridge1, Bridge2, Bridge3, Bridge4, Bridge5, Bridge6, Bridge7, Bridge8, Bridge9, Bridge10,
		Trunnion, Barge,

		_,

		x, y, origin,
		__,

		// unamed anchors
		tail, bridge,
	};

	static void align_label(Tracklet<TSD>* target, TSD id, Labellet* label, float ahsize, float csize) {
		GraphletAnchor a = GraphletAnchor::CC;
		float xoff = 0.0F;
		float yoff = 0.0F;

		switch (id) {
		case TSD::Body2: case TSD::Body3: a = GraphletAnchor::RC; break;
		case TSD::Body4: case TSD::Bridge2: case TSD::Bridge3: case TSD::Hopper2: a = GraphletAnchor::LB; break;
		case TSD::Body1: case TSD::Body5: case TSD::Bridge4: case TSD::Bridge5: case TSD::Hopper3: a = GraphletAnchor::RB; break;
		case TSD::Body6: case TSD::Body7: a = GraphletAnchor::LC; break;
		case TSD::Bridge6: case TSD::Bridge7: case TSD::Bridge8: case TSD::Hopper4: a = GraphletAnchor::RT; break;
		case TSD::Bridge1: case TSD::Bridge9: case TSD::Bridge10: case TSD::Hopper1: a = GraphletAnchor::LT; break;
		case TSD::x: a = GraphletAnchor::CB; yoff = ahsize * -0.5F; break;
		case TSD::y: a = GraphletAnchor::LC; xoff = ahsize * +0.5F; break;
		case TSD::PS_Suction: a = GraphletAnchor::RC; xoff = csize * -0.5F; break;
		case TSD::SB_Suction: a = GraphletAnchor::LC; xoff = csize * +0.5F; break;
		case TSD::origin: a = GraphletAnchor::CT; break;
		}

		target->map_graphlet_at_anchor(label, id, a, xoff, yoff);
	}

	private class SketchMap : public Planet {
	public:
		SketchMap() : Planet("tailing_suction_hopper_dredger_sketch") {}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override {
			float stepsize = 36.0F;
			float thickness = 1.0F;

			this->decorates[TSD::x] = this->insert_one(new ArrowHeadlet(8.0F, -90.0, axes_color));
			this->decorates[TSD::y] = this->insert_one(new ArrowHeadlet(8.0F, 00.0, axes_color));
			this->decorates[TSD::PS_Suction] = this->insert_one(new Circlelet(3.0F, 0xFF0000));
			this->decorates[TSD::SB_Suction] = this->insert_one(new Circlelet(3.0F, 0x00FF00));
			this->decorates[TSD::GPS1] = this->insert_one(new Circlelet(3.0F, 0x0000FF));
			this->decorates[TSD::GPS2] = this->insert_one(new Circlelet(3.0F, 0x0000FF));
			this->decorates[TSD::Barge] = this->insert_one(new Circlelet(4.0F, 0xDDDDDD));
			
			{ // load body sketch
				Turtle<TSD>* body = new Turtle<TSD>(stepsize, false);
				TSD axes[] = { TSD::x, TSD::origin, TSD::y };
				TSD hoppers[] = { TSD::Hopper1, TSD::Hopper2, TSD::Hopper3, TSD::Hopper4, TSD::Hopper1 };

				body->move_down(1.0F, TSD::Body1)->move_left_down(2.0F, 1.0F, TSD::Body2);
				body->move_down(3.0F, TSD::PS_Suction)->move_down(1.0F, TSD::Barge)->move_down(2.0F, TSD::Body3);
				body->move_right_down(1.0F, 0.5F, TSD::Body4)->move_right(1.0F, TSD::tail)->move_right(1.0F, TSD::Body5);
				body->move_right_up(1.0F, 0.5F, TSD::Body6)->move_up(3.0F, TSD::SB_Suction)->move_up(3.0F, TSD::Body7);
				body->move_to(TSD::Body1)->move_down(0.5F, TSD::bridge)->move_down(1.5F, TSD::GPS1)->move_down(3.5F, TSD::GPS2);
				body->move_to(TSD::tail)->move_down(1.0F);

				body->jump_left_up(1.2F, 5.0F, TSD::Hopper1)->move_down(3.0F, TSD::Hopper2)->move_right(2.4F, TSD::Hopper3);
				body->move_up(3.0F, TSD::Hopper4)->move_to(TSD::Hopper1);

				body->jump_back(TSD::Body5)->move_right(2.0F, TSD::y);
				body->jump_back(TSD::Body4)->move_left(2.0F, TSD::origin)->move_up(8.0F, TSD::x);

				this->body = this->insert_one(new Tracklet(body, thickness, sketch_color));
				this->body->push_subtrack(axes, axes_color);
				this->body->push_subtrack(TSD::Body4, TSD::Body5, sketch_color);
				this->body->push_subtrack(hoppers, hopper_color);
			}

			{ // load bridge sketch
				Turtle<TSD>* bridge = new Turtle<TSD>(stepsize, false, TSD::Bridge3);
				
				bridge->move_left_up(1.0F, 0.5F, TSD::Bridge2)->move_up(1.0F, TSD::Bridge1);
				bridge->move_right_up(2.0F, 0.5F, TSD::Bridge10)->move_right_up(0.5F, 0.5F, TSD::Bridge9)->move_right(1.0F, TSD::Bridge8);
				bridge->move_right_down(0.5F, 0.5F, TSD::Bridge7)->move_right_down(2.0F, 0.5F, TSD::Bridge6)->move_down(1.0F, TSD::Bridge5);
				bridge->move_left_down(1.0F, 0.5F, TSD::Bridge4)->move_to(TSD::Bridge3);

				this->bridge = this->insert_one(new Tracklet(bridge, thickness, bridge_color));
			}

			{ // load labels
				unsigned int body0 = _I(TSD::Body1) - 1;
				unsigned int bridge0 = _I(TSD::Bridge1) - 1;
				unsigned int hopper0 = _I(TSD::Hopper1) - 1;

				for (TSD id = _E0(TSD); id < TSD::__; id++) {
					if (id < TSD::_) {
						if ((id >= TSD::Bridge1) && (id <= TSD::Bridge10)) {
							this->bridge_labels[id] = this->insert_one(new Labellet((_I(id) - bridge0).ToString(), sketch_number_font, bridge_color));
						} else if ((id >= TSD::Body1) && (id <= TSD::Body7)) {
							this->body_labels[id] = this->insert_one(new Labellet((_I(id) - body0).ToString(), sketch_number_font, sketch_color));
						} else if ((id >= TSD::Hopper1) && (id <= TSD::Hopper4)) {
							this->body_labels[id] = this->insert_one(new Labellet((_I(id) - hopper0).ToString(), sketch_number_font, hopper_color));
						} else {
							this->body_labels[id] = this->insert_one(new Labellet(_speak(id), sketch_font, sketch_color));
						}
					} else if (id > TSD::_) {
						this->body_labels[id] = this->insert_one(new Labellet(_speak(id), sketch_number_font, axes_color));
					}
				}
			}
		}
		
		void reflow(float width, float height) override {
			float ahsize, csize, osize;

			this->decorates[TSD::y]->fill_extent(0.0F, 0.0F, &ahsize, nullptr);
			this->decorates[TSD::PS_Suction]->fill_extent(0.0F, 0.0F, &csize, nullptr);
			this->body_labels[TSD::origin]->fill_extent(0.0F, 0.0F, &osize, nullptr);

			this->move_to(this->body, 0.0F, 0.0F, GraphletAnchor::LT, flmax(ahsize, osize) * 0.5F);
			this->body->map_graphlet_at_anchor(this->bridge, TSD::bridge, GraphletAnchor::CT);

			for (auto it = this->decorates.begin(); it != this->decorates.end(); it++) {
				switch (it->first) {
				case TSD::GPS1: this->body->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::RC, csize * -2.0F); break;
				case TSD::GPS2: this->body->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, csize * +2.0F); break;
				case TSD::Barge: this->body->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, csize); break;
				default: this->body->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::CC);
				}
			}

			for (auto it = this->body_labels.begin(); it != this->body_labels.end(); it++) {
				align_label(this->body, it->first, it->second, ahsize, csize);
			}

			for (auto it = this->bridge_labels.begin(); it != this->bridge_labels.end(); it++) {
				align_label(this->bridge, it->first, it->second, ahsize, csize);
			}

			this->move_to(this->body_labels[TSD::GPS1], this->decorates[TSD::GPS1], GraphletAnchor::LC, GraphletAnchor::RC, -2.0F);
			this->move_to(this->body_labels[TSD::GPS2], this->decorates[TSD::GPS2], GraphletAnchor::RC, GraphletAnchor::LC, +2.0F);
			this->move_to(this->body_labels[TSD::Barge], this->decorates[TSD::Barge], GraphletAnchor::CB, GraphletAnchor::CT);

			// TODO: The trunnion is probably not present, move its label out of the boundary meanwhile
			this->move_to(this->body_labels[TSD::Trunnion], 0.0F, 0.0F, GraphletAnchor::RB);
		}

	private: // never delete these graphlet manually
		Tracklet<TSD>* body;
		Tracklet<TSD>* bridge;
		std::map<TSD, Labellet*> body_labels;
		std::map<TSD, Labellet*> bridge_labels;
		std::map<TSD, Shapelet*> decorates;
	};
}

private class WarGrey::SCADA::TrailingSuctionDredgerSelf {
public:
	TrailingSuctionDredgerSelf(TrailingSuctionDredgerPlanet* master, Platform::String^ vessel) : master(master), label_max_width(0.0F) {
		this->input_style = make_highlight_dimension_style(label_font->FontSize, 7U, 1U);
		this->input_style.unit_color = label_color;

		this->vessel = vessel;
		this->up_to_date = true;
		this->entity = nullptr;
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height, float inset) {
		this->X[0] = this->master->insert_one(new Labellet("X", label_font, label_color));
		this->X[1] = this->master->insert_one(new Labellet("X", label_font, label_color));
		this->Y[0] = this->master->insert_one(new Labellet("Y", label_font, label_color));
		this->Y[1] = this->master->insert_one(new Labellet("Y", label_font, label_color));

		for (TSD id = _E0(TSD); id < TSD::_; id++) {
			this->labels[id] = this->insert_label(id);
			this->xs[id] = this->insert_input_field(id, 0.0);
			this->ys[id] = this->insert_input_field(id, 0.0);
		}

		this->sketch = this->master->insert_one(new Planetlet(new SketchMap()));
		
		{ /** WARNING
		   * Although TrailingSuctionDredgerlet is an asynchronouse graphlet, it has probably been loaded already,
		   *  thus, the `Planet::on_graghlet_ready()` might be invoked before `Planet::insert()` returns
		   *  in which case `this->dredger` is still `nullptr` if these two statements are combined.
		   *
		   * Also see `this->on_graphlet_ready()`, it checks the graphlet type with `this->dredger == g` instead of dynamic casting.
		   */
			this->dredger = new TrailingSuctionDredgerlet(this->vessel, 1.2F);
			this->master->insert(this->dredger);
		}
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

		this->master->move_to(this->sketch, frame, GraphletAnchor::RT, GraphletAnchor::RT, -inset, inset);
		this->master->move_to(this->dredger, this->sketch, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, inset);
	}

	void on_graphlet_ready(IGraphlet* g) {
		if (this->dredger == g) { // also see `this->load()`
			this->entity = this->dredger->clone_vessel(this->entity, true);
			this->refresh_input_fields();
		}
	}

public:
	bool on_edit(Credit<Dimensionlet, TSD>* dim) {
		long double new_value = dim->get_input_number();
		bool modified = (new_value != dim->get_value());

		if (modified) {
			dim->set_value(new_value);

			this->up_to_date = false;
			this->refresh_entity();

			this->dredger->moor(GraphletAnchor::RB);
			this->dredger->preview(this->entity);
			this->dredger->notify_updated();
		}

		return modified;
	}

	void on_apply() {
		this->refresh_entity();
		this->dredger->refresh(this->entity);
	}

	void on_reset() {
		if (this->dredger != nullptr) {
			this->dredger->preview(nullptr);
			this->dredger->notify_updated();

			this->entity = this->dredger->clone_vessel(this->entity, true);
			this->refresh_input_fields();
		}
	}

public:
	IGraphlet* thumbnail() {
		return this->sketch;
	}

private:
	void refresh_entity() {
		if (this->entity == nullptr) {
			this->entity = ref new TrailingSuctionDredger();
		}

		if (!this->up_to_date) {
			this->up_to_date = true;

			Vessel_Refresh_Vertex(this->entity, gps[0], this->xs, this->ys, TSD::GPS1);
			Vessel_Refresh_Vertex(this->entity, gps[1], this->xs, this->ys, TSD::GPS2);
			Vessel_Refresh_Vertex(this->entity, ps_suction, this->xs, this->ys, TSD::PS_Suction);
			Vessel_Refresh_Vertex(this->entity, sb_suction, this->xs, this->ys, TSD::SB_Suction);
			Vessel_Refresh_Vertex(this->entity, trunnion, this->xs, this->ys, TSD::Trunnion);
			Vessel_Refresh_Vertex(this->entity, barge, this->xs, this->ys, TSD::Barge);

			Vessel_Refresh_Vertex(this->entity, body_vertexes[0], this->xs, this->ys, TSD::Body1);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[1], this->xs, this->ys, TSD::Body2);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[2], this->xs, this->ys, TSD::Body3);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[3], this->xs, this->ys, TSD::Body4);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[4], this->xs, this->ys, TSD::Body5);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[5], this->xs, this->ys, TSD::Body6);
			Vessel_Refresh_Vertex(this->entity, body_vertexes[6], this->xs, this->ys, TSD::Body7);

			Vessel_Refresh_Vertex(this->entity, hopper_vertexes[0], this->xs, this->ys, TSD::Hopper1);
			Vessel_Refresh_Vertex(this->entity, hopper_vertexes[1], this->xs, this->ys, TSD::Hopper2);
			Vessel_Refresh_Vertex(this->entity, hopper_vertexes[2], this->xs, this->ys, TSD::Hopper3);
			Vessel_Refresh_Vertex(this->entity, hopper_vertexes[3], this->xs, this->ys, TSD::Hopper4);

			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[0], this->xs, this->ys, TSD::Bridge1);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[1], this->xs, this->ys, TSD::Bridge2);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[2], this->xs, this->ys, TSD::Bridge3);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[3], this->xs, this->ys, TSD::Bridge4);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[4], this->xs, this->ys, TSD::Bridge5);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[5], this->xs, this->ys, TSD::Bridge6);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[6], this->xs, this->ys, TSD::Bridge7);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[7], this->xs, this->ys, TSD::Bridge8);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[8], this->xs, this->ys, TSD::Bridge9);
			Vessel_Refresh_Vertex(this->entity, bridge_vertexes[9], this->xs, this->ys, TSD::Bridge10);
		}
	}

	void refresh_input_fields() {
		if (this->entity != nullptr) {
			this->up_to_date = true;

			this->master->begin_update_sequence();

			Vessel_Display_Vertex(this->entity, gps[0], this->xs, this->ys, TSD::GPS1);
			Vessel_Display_Vertex(this->entity, gps[1], this->xs, this->ys, TSD::GPS2);
			Vessel_Display_Vertex(this->entity, ps_suction, this->xs, this->ys, TSD::PS_Suction);
			Vessel_Display_Vertex(this->entity, sb_suction, this->xs, this->ys, TSD::SB_Suction);
			Vessel_Display_Vertex(this->entity, trunnion, this->xs, this->ys, TSD::Trunnion);
			Vessel_Display_Vertex(this->entity, barge, this->xs, this->ys, TSD::Barge);

			Vessel_Display_Vertex(this->entity, body_vertexes[0], this->xs, this->ys, TSD::Body1);
			Vessel_Display_Vertex(this->entity, body_vertexes[1], this->xs, this->ys, TSD::Body2);
			Vessel_Display_Vertex(this->entity, body_vertexes[2], this->xs, this->ys, TSD::Body3);
			Vessel_Display_Vertex(this->entity, body_vertexes[3], this->xs, this->ys, TSD::Body4);
			Vessel_Display_Vertex(this->entity, body_vertexes[4], this->xs, this->ys, TSD::Body5);
			Vessel_Display_Vertex(this->entity, body_vertexes[5], this->xs, this->ys, TSD::Body6);
			Vessel_Display_Vertex(this->entity, body_vertexes[6], this->xs, this->ys, TSD::Body7);

			Vessel_Display_Vertex(this->entity, hopper_vertexes[0], this->xs, this->ys, TSD::Hopper1);
			Vessel_Display_Vertex(this->entity, hopper_vertexes[1], this->xs, this->ys, TSD::Hopper2);
			Vessel_Display_Vertex(this->entity, hopper_vertexes[2], this->xs, this->ys, TSD::Hopper3);
			Vessel_Display_Vertex(this->entity, hopper_vertexes[3], this->xs, this->ys, TSD::Hopper4);

			Vessel_Display_Vertex(this->entity, bridge_vertexes[0], this->xs, this->ys, TSD::Bridge1);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[1], this->xs, this->ys, TSD::Bridge2);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[2], this->xs, this->ys, TSD::Bridge3);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[3], this->xs, this->ys, TSD::Bridge4);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[4], this->xs, this->ys, TSD::Bridge5);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[5], this->xs, this->ys, TSD::Bridge6);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[6], this->xs, this->ys, TSD::Bridge7);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[7], this->xs, this->ys, TSD::Bridge8);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[8], this->xs, this->ys, TSD::Bridge9);
			Vessel_Display_Vertex(this->entity, bridge_vertexes[9], this->xs, this->ys, TSD::Bridge10);

			this->master->end_update_sequence();
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
	Platform::String^ vessel;
	bool up_to_date;

private: // never delete these graphlet manually
	TrailingSuctionDredgerlet* dredger;
	Planetlet* sketch;
	Labellet* X[2];
	Labellet* Y[2];
	std::map<TSD, Labellet*> labels;
	std::map<TSD, Credit<Dimensionlet, TSD>*> xs;
	std::map<TSD, Credit<Dimensionlet, TSD>*> ys;
	
private:
	TrailingSuctionDredgerPlanet* master;
};

/*************************************************************************************************/
TrailingSuctionDredgerPlanet::TrailingSuctionDredgerPlanet(Platform::String^ vessel) : EditorPlanet(__MODULE__) {
	this->self = new TrailingSuctionDredgerSelf(this, vessel);
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

void TrailingSuctionDredgerPlanet::on_graphlet_ready(IGraphlet* g) {
	this->self->on_graphlet_ready(g);
}

IGraphlet* TrailingSuctionDredgerPlanet::thumbnail_graphlet() {
	return this->self->thumbnail();
}

void TrailingSuctionDredgerPlanet::on_apply() {
	this->self->on_apply();
}

void TrailingSuctionDredgerPlanet::on_reset() {
	this->self->on_reset();
}

bool TrailingSuctionDredgerPlanet::on_edit(Dimensionlet* dim) {
	return this->self->on_edit(static_cast<Credit<Dimensionlet, TSD>*>(dim));
}
