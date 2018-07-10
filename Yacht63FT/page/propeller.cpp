#include "page/propeller.hpp"
#include "decorator/decorator.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/indicatorlet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class P { Converter, Motor, Bus, Winding, Bearing, _ };
private enum class PConverter { temperature, voltage, current, _ };
private enum class PMoter { power, rspeed, _ };
private enum class PBus { dc_V, storage_V, coolant_C, _ };
private enum class PWinding { Up_C, Vp_C, Wp_C, _ };
private enum class PBearing { drive_C, nondrive_C, thrust_C, _ };

static const unsigned int pcount = 2U;

static const float corner_radius = 8.0F;
static const float label_fy = 0.25F;

private class PDecorator final : public IPlanetDecorator {
public:
	PDecorator(float width, float height, float padding) : region_height(height), region_padding(padding) {
		float metrics_height = design_to_application_height(125.0F);

		this->region_width = (width - padding) / float(pcount) - padding;
		this->cell_gapsize = design_to_application_width(8.0F);
		this->cell_margin = (metrics_height - design_to_application_height(102.0F)) * 0.5F;

		this->cell_color = Colours::make(0x131615U);
		this->fgcolors[P::Converter] = Colours::make(0x878787U);
		this->fgcolors[P::Motor] = Colours::make(0x919191U);
		this->fgcolors[P::Motor] = Colours::make(0x878787U);
		this->fgcolors[P::Bus] = Colours::make(0x878787U);
		this->fgcolors[P::Winding] = Colours::make(0x878787U);
		this->fgcolors[P::Bearing] = Colours::make(0x878787U);
		this->bgcolors[P::Converter] = Colours::make(0x313131U);
		this->bgcolors[P::Motor] = Colours::make(0x292929U);
		this->bgcolors[P::Bus] = Colours::make(0x272727U);
		this->bgcolors[P::Winding] = Colours::make(0x252525U);
		this->bgcolors[P::Bearing] = Colours::make(0x232323U);
		
		for (P region = _E0(P); region < P::_; region++) {
			if (region == P::Motor) {
				float rc = _F(P::_);

				this->heights[region] = height - metrics_height * (rc - 1.0F) - this->region_padding * (rc + 1.0F);
			} else {
				this->heights[region] = metrics_height;
			}
		}

		this->ys[P::Converter] = this->region_padding;
		for (unsigned int region = 1; region < _N(P); region++) {
			P prev = _E(P, region - 1);

			this->ys[_E(P, region)] = this->ys[prev] + this->heights[prev] + this->region_padding;
		}

		{ // initialize labels
			CanvasTextFormat^ pfont = make_text_format("Microsoft YaHei", design_to_application_height(30.0F));
			CanvasTextFormat^ gfont = make_text_format("Microsoft YaHei", design_to_application_height(24.0F));

			for (unsigned int id = 1; id <= pcount; id++) {
				this->cids[id - 1] = make_text_layout("M" + id.ToString(), pfont);
			}

			for (PConverter m = _E0(PConverter); m < PConverter::_; m++) {
				this->cs[m] = make_text_layout(speak(m, "cnvt"), pfont);
			}

			for (PMoter m = _E0(PMoter); m < PMoter::_; m++) {
				this->ms[m] = make_text_layout(speak(m, "pm"), gfont);
			}

			for (PBus m = _E0(PBus); m < PBus::_; m++) {
				this->dcbs[m] = make_text_layout(speak(m), pfont);
			}

			for (PWinding m = _E0(PWinding); m < PWinding::_; m++) {
				this->ws[m] = make_text_layout(speak(m), pfont);
			}

			for (PBearing m = _E0(PBearing); m < PBearing::_; m++) {
				this->bs[m] = make_text_layout(speak(m), pfont);
			}
		}
	}

public:
	void draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < pcount; idx++) {
			this->draw_region(ds, idx);
		}
	}

	void draw_after(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < pcount; idx++) {
			this->draw_region_label(ds, idx);
		}
	}

public:
	template<typename M>
	void fill_metrics_cell_extent(unsigned int p_idx, P p, M m, float* x, float* y, float* width, float* height) {
		float flcount = _F(M::_);
		float cell_height = (this->heights[p] - this->cell_margin * 2.0F);
		float cell_width = (this->region_width - this->cell_margin * 2.0F - this->cell_gapsize * (flcount - 1.0F)) / flcount;
		float left_x = (this->region_width + this->region_padding) * float(p_idx) + this->region_padding;
		float cell_x = left_x + this->cell_margin + (cell_width + this->cell_gapsize) * _F(m);
		float cell_y = this->ys[p] + this->cell_margin;

		SET_VALUES(x, cell_x, y, cell_y);
		SET_VALUES(width, cell_width, height, cell_height);
	}

	template<typename M>
	void fill_cell_anchor(unsigned int p_idx, P m, M p, float fw, float fh, float* x, float* y, float* width = nullptr, float* height = nullptr) {
		float cell_x, cell_y, cell_width, cell_height;

		this->fill_metrics_cell_extent(p_idx, m, p, &cell_x, &cell_y, &cell_width, &cell_height);

		SET_BOX(x, (cell_x + cell_width * fw));
		SET_BOX(y, (cell_y + cell_height * fh));
		SET_BOX(width, cell_width);
		SET_BOX(height, cell_height);
	}

private:
	float region_x(unsigned int p_idx) {
		return (this->region_width + this->region_padding) * float(p_idx) + this->region_padding;
	}

	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float x = this->region_x(idx);

		for (P region = _E0(P); region < P::_; region++) {
			float y = this->ys[region];
			float height = this->heights[region];
			ICanvasBrush^ color = this->bgcolors[region];

			ds->FillRectangle(x, y, this->region_width, height, color);

			switch (region) {
			case P::Converter: this->draw_cells(ds, idx, region, PConverter::_); break;
			case P::Motor: this->draw_cells(ds, idx, region, PMoter::_); break;
			case P::Bus: this->draw_cells(ds, idx, region, PBus::_); break;
			case P::Winding: this->draw_cells(ds, idx, region, PWinding::_); break;
			case P::Bearing: this->draw_cells(ds, idx, region, PBearing::_); break;
			}
		}
	}

	void draw_region_label(CanvasDrawingSession^ ds, unsigned int idx) {
		float anchor_x, anchor_y;

		this->draw_labels(ds, idx, P::Converter, this->cs, this->fgcolors[P::Converter], this->cids[idx]);
		
		for (PMoter g = _E0(PMoter); g < PMoter::_; g++) {
			this->fill_cell_anchor(idx, P::Motor, g, 0.5F, 0.51F, &anchor_x, &anchor_y);

			anchor_x -= this->ms[g]->LayoutBounds.Width * 0.5F;
			ds->DrawTextLayout(this->ms[g], anchor_x, anchor_y, this->fgcolors[P::Motor]);
		}

		this->draw_labels(ds, idx, P::Bus, this->dcbs, this->fgcolors[P::Bus]);
		this->draw_labels(ds, idx, P::Winding, this->ws, this->fgcolors[P::Winding]);
		this->draw_labels(ds, idx, P::Bearing, this->bs, this->fgcolors[P::Bearing]);
	}

	template<typename M_>
	void draw_cells(CanvasDrawingSession^ ds, unsigned int idx, P region, M_ m_) {
		float cell_x, cell_y, cell_width, cell_height;

		for (M_ m = _E0(M_); m < m_; m++) {
			this->fill_metrics_cell_extent(idx, region, m, &cell_x, &cell_y, &cell_width, &cell_height);
			ds->FillRoundedRectangle(cell_x, cell_y, cell_width, cell_height, corner_radius, corner_radius, this->cell_color);
		}
	}

	template<typename M>
	void draw_labels(CanvasDrawingSession^ ds, unsigned int idx, P region, std::map<M, CanvasTextLayout^> src, ICanvasBrush^ fgcolor, CanvasTextLayout^ id = nullptr) {
		float anchor_x, anchor_y;
		
		for (M m = _E0(M); m < M::_; m++) {
			this->fill_cell_anchor(idx, region, m, 0.10F, label_fy, &anchor_x, &anchor_y);
			anchor_y -= src[m]->LayoutBounds.Height * 0.5F;

			if ((id != nullptr) && (_I(m) == 0)) {
				ds->DrawTextLayout(id, anchor_x, anchor_y, fgcolor);
				ds->DrawTextLayout(src[m], anchor_x + id->LayoutBounds.Width, anchor_y, fgcolor);
			} else {
				ds->DrawTextLayout(src[m], anchor_x, anchor_y, fgcolor);
			}
		}
	}

private:
	CanvasTextLayout^ cids[pcount];
	std::map<PConverter, CanvasTextLayout^> cs;
	std::map<PMoter, CanvasTextLayout^> ms;
	std::map<PBus, CanvasTextLayout^> dcbs;
	std::map<PWinding, CanvasTextLayout^> ws;
	std::map<PBearing, CanvasTextLayout^> bs;

private:
	ICanvasBrush^ cell_color;
	std::map<P, ICanvasBrush^> fgcolors;
	std::map<P, ICanvasBrush^> bgcolors;

private:
	float region_width;
	float region_height;
	float region_padding;
	float cell_gapsize;
	float cell_margin;
	std::map<P, float> ys;
	std::map<P, float> heights;
};

/*************************************************************************************************/
private class PBoard final : public PLCConfirmation {
public:
	~PBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	PBoard(PropellerPage* master, PDecorator* decorator) : master(master), decorator(decorator) {
		Platform::String^ scale_face = "Arial";
		this->metrics_fonts[0] = make_text_format(scale_face, design_to_application_height(42.0F));
		this->metrics_fonts[1] = make_text_format(scale_face, design_to_application_height(37.5F));
		this->gauge_fonts[0] = make_text_format(scale_face, design_to_application_height(32.0F));
		this->gauge_fonts[1] = make_text_format(scale_face, design_to_application_height(28.00F));

		this->fgcolor = Colours::make(0xD2D2D2);

		this->decorator->reference();
	}

public:
	void load_and_flow() {
		float anchor_x, anchor_y, cell_width, cell_height;
		float dim_fx = 0.90F;
		float dim_fy = 0.75F;

		for (unsigned int idx = 0; idx < pcount; idx++) {	
			for (PConverter c = _E0(PConverter); c < PConverter::_; c++) {
				this->decorator->fill_cell_anchor(idx, P::Converter, c, dim_fx, dim_fy, &anchor_x, &anchor_y);
				this->cs[c][idx] = this->master->insert_one(make_dimension(c.ToString()), anchor_x, anchor_y, GraphletAnchor::RC);
			}

			for (PMoter m = _E0(PMoter); m < PMoter::_; m++) {
				Platform::String^ unit = "<" + m.ToString() + ">";

				this->ms[m][idx] = new Dimensionlet(unit, this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);

				this->decorator->fill_cell_anchor(idx, P::Motor, m, 0.5F, 0.5F, &anchor_x, &anchor_y, &cell_width, &cell_height);
				this->gs[m][idx] = new Indicatorlet(std::fminf(cell_width, cell_height) * 0.8F, indicator_thickness);

				this->master->insert(this->gs[m][idx], anchor_x, anchor_y, GraphletAnchor::CC);
				this->master->insert(this->ms[m][idx], anchor_x, anchor_y, GraphletAnchor::CB);
			}

			for (PBus b = _E0(PBus); b < PBus::_; b++) {
				this->decorator->fill_cell_anchor(idx, P::Bus, b, dim_fx, dim_fy, &anchor_x, &anchor_y);
				this->dcbs[b][idx] = this->master->insert_one(make_dimension(b.ToString()), anchor_x, anchor_y, GraphletAnchor::RC);
			}

			for (PWinding w = _E0(PWinding); w < PWinding::_; w++) {
				this->decorator->fill_cell_anchor(idx, P::Winding, w, dim_fx, dim_fy, &anchor_x, &anchor_y);
				this->ws[w][idx] = this->master->insert_one(make_dimension(w.ToString()), anchor_x, anchor_y, GraphletAnchor::RC);
			}

			for (PBearing b = _E0(PBearing); b < PBearing::_; b++) {
				this->decorator->fill_cell_anchor(idx, P::Bearing, b, dim_fx, dim_fy, &anchor_x, &anchor_y);
				this->bs[b][idx] = this->master->insert_one(make_dimension(b.ToString()), anchor_x, anchor_y, GraphletAnchor::RC);
			}
		}
	}

public:
	void on_analog_input_data(uint8* db4, size_t size, Syslog* logger) override {
		GraphletAnchor rb = GraphletAnchor::RB;
		size_t db_idx_acc = 11;
		
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->set_motor_meters(PMoter::power,  db4, 24U, db_idx_acc);
		this->set_motor_meters(PMoter::rspeed, db4, 25U, db_idx_acc);

		this->set_values(this->cs[PConverter::current],     db4, 26U, db_idx_acc, rb);
		this->set_values(this->cs[PConverter::voltage],     db4, 27U, db_idx_acc, rb);
		this->set_values(this->cs[PConverter::temperature], db4, 28U, db_idx_acc, rb);
		this->set_values(this->ws[PWinding::Up_C],          db4, 29U, db_idx_acc, rb);
		this->set_values(this->ws[PWinding::Vp_C],          db4, 30U, db_idx_acc, rb);
		this->set_values(this->ws[PWinding::Wp_C],          db4, 31U, db_idx_acc, rb);
		this->set_values(this->bs[PBearing::drive_C],       db4, 32U, db_idx_acc, rb);
		this->set_values(this->bs[PBearing::nondrive_C],    db4, 33U, db_idx_acc, rb);
		this->set_values(this->bs[PBearing::thrust_C],      db4, 34U, db_idx_acc, rb);
		this->set_values(this->dcbs[PBus::dc_V],            db4, 73U, 1,  rb);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	template<class G>
	void set_values(G* gs[], uint8* db, size_t idx0, size_t acc, GraphletAnchor a) {
		for (unsigned int idx = 0; idx < pcount; idx++) {
			gs[idx]->set_value(AI_ref(db, idx0 + acc * idx), a);
		}
	}

	void set_motor_meters(PMoter m, uint8* db, size_t idx0, size_t acc) {
		for (unsigned int idx = 0; idx < pcount; idx++) {
			float v = AI_ref(db, idx0 + acc * idx);

			this->ms[m][idx]->set_value(v, GraphletAnchor::CC);
			this->gs[m][idx]->set_value(v);
		}
	}

private:
	Dimensionlet* make_dimension(Platform::String^ dimension) {
		Platform::String^ unit = "<" + dimension + ">";

		switch ((dimension->Data())[dimension->Length() - 1]) {
		case L'V': unit = "<voltage>"; break;
		case L'C': unit = "<temperature>"; break;
		}

		return new Dimensionlet(unit, this->metrics_fonts[0], this->metrics_fonts[1], this->fgcolor);
	}

// never deletes these graphlets mannually
private:
	std::map<PConverter, Dimensionlet*[pcount]> cs;
	std::map<PBus, Dimensionlet*[pcount]> dcbs;
	std::map<PWinding, Dimensionlet*[pcount]> ws;
	std::map<PBearing, Dimensionlet*[pcount]> bs;
	std::map<PMoter, Dimensionlet*[pcount]> ms;
	std::map<PMoter, Indicatorlet*[pcount]> gs;
		
private:
	CanvasTextFormat^ metrics_fonts[2];
	CanvasTextFormat^ gauge_fonts[2];
	ICanvasBrush^ fgcolor;
	PropellerPage* master;
	PDecorator* decorator;
};

/*************************************************************************************************/
PropellerPage::PropellerPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

PropellerPage::~PropellerPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void PropellerPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		PDecorator* regions = new PDecorator(width, height, design_to_application_height(4.0F));
		PBoard* gb = new PBoard(this, regions);

		gb->load_and_flow();

		this->dashboard = gb;
		this->set_decorator(regions);
		this->device->append_confirmation_receiver(gb);
	}
}

void PropellerPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
