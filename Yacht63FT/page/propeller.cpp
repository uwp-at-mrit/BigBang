#include "page/propeller.hpp"
#include "decorator/decorator.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/indicatorlet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "msappx.hxx"
#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class P { Power, Gauge, _ };
private enum class PPower { power, speed, current, _ };
private enum class PGauge { cnvt_V, cnvt_C, Upw_C, Vpw_C, Wpw_C, deb_C, ndeb_C, stern_C, _ };

static const unsigned int pcount = 2U;

static const float corner_radius = 8.0F;
static const float label_fy = 0.25F;

private class PDecorator final : public IPlanetDecorator {
public:
	PDecorator(float width, float height, float padding) : region_height(height), region_padding(padding) {
		this->region_width = (width - padding) / float(pcount) - padding;
		this->cell_gapsize = design_to_application_width(8.0F);

		this->cell_color = Colours::make(0x131615U);
		this->fgcolors[P::Power] = Colours::make(0x878787U);
		this->fgcolors[P::Gauge] = Colours::make(0x919191U);
		this->bgcolors[P::Power] = Colours::make(0x313131U);
		this->bgcolors[P::Gauge] = Colours::make(0x313131U);
		
		this->heights[P::Power] = design_to_application_height(125.0F);
		this->heights[P::Gauge] = height - this->heights[P::Power] - this->region_padding * (static_cast<float>(P::_) + 1.0F);

		this->ys[P::Power] = this->region_padding;
		for (unsigned int region = 1; region < static_cast<unsigned int>(P::_); region++) {
			P prev = static_cast<P>(region - 1);

			this->ys[static_cast<P>(region)] = this->ys[prev] + this->heights[prev] + this->region_padding;
		}

		{ // initialize labels
			CanvasTextFormat^ pfont = make_text_format("Microsoft YaHei", design_to_application_height(30.0F));
			CanvasTextFormat^ mfont = make_text_format("Microsoft YaHei", design_to_application_height(24.0F));

			for (PPower p = static_cast<PPower>(0); p < PPower::_; p++) {
				this->powers[p] = make_text_layout(speak(":" + p.ToString() + ":"), pfont);
			}

			for (PGauge m = static_cast<PGauge>(0); m < PGauge::_; m++) {
				this->gauges[m] = make_text_layout(speak(m.ToString()), mfont);
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
	void fill_power_cell_extent(unsigned int g_idx, PPower p, float* x, float* y, float* width, float* height) {
		static float cell_height = design_to_application_height(102.0F);
		static float cell_margin = (this->heights[P::Power] - cell_height) * 0.5F;
		static float cell_width = (this->region_width - cell_margin * 2.0F - cell_gapsize * 2.0F) / 3.0F;

		float left_x = (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
		float cell_x = left_x + cell_margin + (cell_width + cell_gapsize) * static_cast<float>(p);
		float cell_y = this->ys[P::Power] + cell_margin;

		SET_VALUES(x, cell_x, y, cell_y);
		SET_VALUES(width, cell_width, height, cell_height);
	}

	void fill_gauge_cell_extent(unsigned int g_idx, PGauge g, float* x, float* y, float* width, float* height) {
		static unsigned int total = static_cast<unsigned int>(PGauge::_);
		static unsigned int cols = 3;
		static unsigned int mod = total % cols;
		static unsigned int rows = total / cols + ((mod == 0) ? 0 : 1);
		static float cell_height = (this->heights[P::Gauge] - this->cell_gapsize * float(rows + 1)) / float(rows);
		static float cell_swidth = (this->region_width - this->cell_gapsize * float(cols + 1)) / float(cols);
		static float cell_lwidth = ((mod == 0) ? cell_swidth : (this->region_width - this->cell_gapsize * float(mod + 1)) / float(mod));

		unsigned int id = static_cast<unsigned int>(g);
		float cell_width = cell_swidth;
		float left_x = (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
		float cell_x = left_x + this->cell_gapsize;
		float cell_y = this->ys[P::Gauge] + this->cell_gapsize;

		if (id < mod) {
			cell_width = cell_lwidth;
			cell_x += ((cell_width + cell_gapsize) * float(id));
		} else {
			cell_x += ((cell_width + this->cell_gapsize)  * float((id - mod) % cols));
			cell_y += ((cell_height + this->cell_gapsize) * float((id - mod) / cols + 1));
		}

		SET_VALUES(x, cell_x, y, cell_y);
		SET_VALUES(width, cell_width, height, cell_height);
	}

	void fill_power_anchor(unsigned int g_idx, PPower p, float fw, float fh, float* x, float* y) {
		float cell_x, cell_y, cell_width, cell_height;

		this->fill_power_cell_extent(g_idx, p, &cell_x, &cell_y, &cell_width, &cell_height);

		SET_BOX(x, (cell_x + cell_width * fw));
		SET_BOX(y, (cell_y + cell_height * fh));
	}

	void fill_gauges_anchor(unsigned int g_idx, PGauge g, float fh, float* x, float* y) {
		float cell_x, cell_y, cell_width, cell_height;

		this->fill_gauge_cell_extent(g_idx, g, &cell_x, &cell_y, &cell_width, &cell_height);

		SET_BOX(x, (cell_x + cell_width * 0.5F));
		SET_BOX(y, (cell_y + cell_height * fh));
	}

private:
	float region_x(unsigned int g_idx) {
		return (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
	}

	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float cell_x, cell_y, cell_width, cell_height;
		float x = this->region_x(idx);

		for (P region = static_cast<P>(0); region < P::_; region++) {
			float y = this->ys[region];
			float height = this->heights[region];
			ICanvasBrush^ color = this->bgcolors[region];

			ds->FillRectangle(x, y, this->region_width, height, color);
		}

		for (PPower p = static_cast<PPower>(0); p < PPower::_; p++) {
			this->fill_power_cell_extent(idx, p, &cell_x, &cell_y, &cell_width, &cell_height);
			ds->FillRoundedRectangle(cell_x, cell_y, cell_width, cell_height,
				corner_radius, corner_radius, this->cell_color);
		}

		for (PGauge g = static_cast<PGauge>(0); g < PGauge::_; g++) {
			this->fill_gauge_cell_extent(idx, g, &cell_x, &cell_y, &cell_width, &cell_height);
			ds->FillRoundedRectangle(cell_x, cell_y, cell_width, cell_height,
				corner_radius, corner_radius, this->cell_color);
		}
	}

	void draw_region_label(CanvasDrawingSession^ ds, unsigned int idx) {
		float anchor_x, anchor_y, offset;

		for (PPower p = static_cast<PPower>(0); p < PPower::_; p++) {
			offset = this->powers[p]->LayoutBounds.Height * 0.5F;
			this->fill_power_anchor(idx, p, 0.10F, label_fy, &anchor_x, &anchor_y);
			ds->DrawTextLayout(this->powers[p], anchor_x, anchor_y - offset, this->fgcolors[P::Power]);
		}

		for (PGauge g = static_cast<PGauge>(0); g < PGauge::_; g++) {
			offset = this->gauges[g]->LayoutBounds.Width * 0.5F;
			this->fill_gauges_anchor(idx, g, 0.51F, &anchor_x, &anchor_y);
			ds->DrawTextLayout(this->gauges[g], anchor_x - offset, anchor_y, this->fgcolors[P::Gauge]);
		}
	}

private:
	std::map<PPower, CanvasTextLayout^> powers;
	std::map<PGauge, CanvasTextLayout^> gauges;

private:
	ICanvasBrush^ cell_color;
	std::map<P, ICanvasBrush^> fgcolors;
	std::map<P, ICanvasBrush^> bgcolors;

private:
	float region_width;
	float region_height;
	float region_padding;
	float cell_gapsize;
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
		this->power_fonts[0] = make_text_format(scale_face, design_to_application_height(42.0F));
		this->power_fonts[1] = make_text_format(scale_face, design_to_application_height(37.5F));
		this->gauge_fonts[0] = make_text_format(scale_face, design_to_application_height(32.0F));
		this->gauge_fonts[1] = make_text_format(scale_face, design_to_application_height(28.00F));

		this->fgcolor = Colours::make(0xD2D2D2);

		this->decorator->reference();
	}

public:
	void load_and_flow() {
		float anchor_x, anchor_y, cell_width, cell_height;

		for (unsigned int idx = 0; idx < pcount; idx++) {
			this->decorator->fill_gauge_cell_extent(idx, PGauge::stern_C, nullptr, nullptr, &cell_width, &cell_height);

			for (PPower p = static_cast<PPower>(0); p < PPower::_; p++) {
				Platform::String^ unit = "<" + p.ToString() + ">";

				this->decorator->fill_power_anchor(idx, p, 0.9F, 0.75F, &anchor_x, &anchor_y);
				this->powers[p] = new Dimensionlet(unit, this->power_fonts[0], this->power_fonts[1], this->fgcolor);
				this->master->insert(this->powers[p], anchor_x, anchor_y, GraphletAlignment::RC);
			}

			for (PGauge g = static_cast<PGauge>(0); g < PGauge::_; g++) {
				if (g == PGauge::cnvt_V) {
					this->metrics[g] = new Dimensionlet("<power>", this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);
				} else {
					this->metrics[g] = new Dimensionlet("<celsius>", this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);
				}
				
				this->decorator->fill_gauges_anchor(idx, g, 0.5F, &anchor_x, &anchor_y);
				this->gauges[g] = new Indicatorlet(std::fminf(cell_width, cell_height) * 0.8F);
				
				this->master->insert(this->gauges[g], anchor_x, anchor_y, GraphletAlignment::CC);
				this->master->insert(this->metrics[g], anchor_x, anchor_y, GraphletAlignment::CB);

				this->gauges[g]->set_value(0.5F);
			}
		}
	}

// never deletes these graphlets mannually
private:
	std::map<PPower, Dimensionlet*> powers;
	std::map<PGauge, Dimensionlet*> metrics;
	std::map<PGauge, Indicatorlet*> gauges;
		
private:
	CanvasTextFormat^ power_fonts[2];
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
		PDecorator* regions = new PDecorator(width, height, design_to_application_height(2.0F));
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
