#include "page/generator.hpp"
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

private enum class G { RSpeed, Power, Gauge, Alert, _ };
private enum class GPower { voltage, current, frequency, _ };
private enum class GMeter { sea, oil, coolant, _ };

static const unsigned int gcount = 2U;

static const float corner_radius = 8.0F;
static const float rspeed_label_fx = 0.089F;
static const float label_fy = 0.25F;

private class GDecorator final : public IPlanetDecorator {
public:
	GDecorator(IPlanet* master, float width, float height, float padding)
		: master(master), region_height(height), region_padding(padding) {
		this->region_width = (width - padding) / float(gcount) - padding;

		this->rspeed_bgcolors[0] = Colours::make(0x101410U);
		this->rspeed_bgcolors[1] = Colours::make(0x151915U);
		this->power_cell_color = Colours::make(0x131615U);
		this->fgcolors[G::RSpeed] = Colours::GhostWhite;
		this->fgcolors[G::Power] = Colours::make(0x878787U);
		this->fgcolors[G::Gauge] = Colours::make(0x919191U);
		this->bgcolors[G::Power] = Colours::make(0x313131U);
		this->bgcolors[G::Gauge] = Colours::make(0x1E1E1EU);
		this->bgcolors[G::Alert] = Colours::make(0x131615U);

		this->heights[G::RSpeed] = this->master->sketch_to_application_height(180.0F);
		this->heights[G::Power] = this->master->sketch_to_application_height(125.0F);
		this->heights[G::Alert] = this->master->sketch_to_application_height(70.0F);
		this->heights[G::Gauge] = height
			- (this->heights[G::RSpeed] + this->heights[G::Power] + this->heights[G::Alert])
			- this->region_padding * float(_N(G) + 1);

		this->ys[G::RSpeed] = this->region_padding;
		for (unsigned int region = 1; region < _N(G); region++) {
			G prev = _E(G, region - 1);

			this->ys[_E(G, region)] = this->ys[prev] + this->heights[prev] + this->region_padding;
		}

		{ // initialize labels
			CanvasTextFormat^ sfont = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(33.75F));
			CanvasTextFormat^ pfont = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(30.0F));
			CanvasTextFormat^ mfont = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(20.0F));

			this->rspeed = make_text_layout(speak(":rspeed:"), sfont);

			for (GPower p = _E0(GPower); p < GPower::_; p++) {
				this->powers[p] = make_text_layout(speak(p, nullptr), pfont);
			}

			for (GMeter g = _E0(GMeter); g < GMeter::_; g++) {
				if (g == GMeter::sea) {
					this->foil_filter_pdrop = make_text_layout(speak(":pd_filter:"), mfont);
				} else {
					this->temperatures[g] = make_text_layout(speak(g, "t"), mfont);
				}

				this->pressures[g] = make_text_layout(speak(g, "p"), mfont);
			}
		}
	}

public:
	void draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->draw_region(ds, idx);
		}
	}

	void draw_after(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->draw_region_label(ds, idx);
		}
	}

public:
	void fill_power_cell_extent(unsigned int g_idx, GPower p, float* x, float* y, float* width, float* height) {
		static float cell_gapsize = this->master->sketch_to_application_width(8.0F);
		static float cell_height = this->master->sketch_to_application_height(102.0F);
		static float cell_margin = (this->heights[G::Power] - cell_height) * 0.5F;
		static float cell_width = (this->region_width - cell_margin * 2.0F - cell_gapsize * 2.0F) / 3.0F;

		float left_x = (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
		float cell_x = left_x + cell_margin + (cell_width + cell_gapsize) * _F(p);
		float cell_y = this->ys[G::Power] + cell_margin;

		SET_VALUES(x, cell_x, y, cell_y);
		SET_VALUES(width, cell_width, height, cell_height);
	}

	void fill_rspeed_anchor(unsigned int g_idx, float fw, float fh, float* x, float* y) {
		float anchor_x = this->region_x(g_idx) + this->region_width * fw;
		float anchor_y = this->ys[G::RSpeed] + this->heights[G::RSpeed] * fh;

		SET_VALUES(x, anchor_x, y, anchor_y);
	}

	void fill_power_anchor(unsigned int g_idx, GPower p, float fw, float fh, float* x, float* y) {
		float cell_x, cell_y, cell_width, cell_height;

		this->fill_power_cell_extent(g_idx, p, &cell_x, &cell_y, &cell_width, &cell_height);

		SET_BOX(x, (cell_x + cell_width * fw));
		SET_BOX(y, (cell_y + cell_height * fh));
	}

	void fill_gauges_anchor(unsigned int g_idx, GMeter g, float fh, float* x, float* y, float* cell_size = nullptr) {
		static float mflcount = _F(GMeter::_);
		static float subwidth = this->region_width / mflcount;
		float gauge_x = this->region_x(g_idx) +  subwidth * (_F(g) + 0.5F);
		float gauge_y = this->ys[G::Gauge] + this->heights[G::Gauge] * fh;

		SET_VALUES(x, gauge_x, y, gauge_y);

		if (cell_size != nullptr) {
			(*cell_size) = fmin(subwidth, this->heights[G::Gauge] * 0.5F) * 0.8F;
		}
	}

private:
	float region_x(unsigned int g_idx) {
		return (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
	}

	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float x = this->region_x(idx);

		ds->FillRectangle(x, this->ys[G::RSpeed], this->region_width, this->heights[G::RSpeed], this->rspeed_bgcolors[idx]);

		for (G region = _E(G, 1); region < G::_; region++) {
			float y = this->ys[region];
			float height = this->heights[region];
			ICanvasBrush^ color = this->bgcolors[region];

			if (region != G::Alert) {
				ds->FillRectangle(x, y, this->region_width, height, color);
			} else {
				ds->FillRoundedRectangle(x, y, this->region_width, height, corner_radius, corner_radius, color);
			}
		}

		{ // draw power subregions
			float cell_x, cell_y, cell_width, cell_height;

			for (GPower p = _E0(GPower); p < GPower::_; p++) {
				this->fill_power_cell_extent(idx, p, &cell_x, &cell_y, &cell_width, &cell_height);
				ds->FillRoundedRectangle(cell_x, cell_y, cell_width, cell_height,
					corner_radius, corner_radius, this->power_cell_color);
			}
		}
	}

	void draw_region_label(CanvasDrawingSession^ ds, unsigned int idx) {
		float offset = this->rspeed->LayoutBounds.Height * 0.5F;
		float anchor_x, anchor_y;

		this->fill_rspeed_anchor(idx, rspeed_label_fx, label_fy, &anchor_x, &anchor_y);
		ds->DrawTextLayout(this->rspeed, anchor_x, anchor_y - offset, this->fgcolors[G::RSpeed]);

		for (GPower p = _E0(GPower); p < GPower::_; p++) {
			offset = this->powers[p]->LayoutBounds.Height * 0.5F;
			this->fill_power_anchor(idx, p, 0.10F, label_fy, &anchor_x, &anchor_y);
			ds->DrawTextLayout(this->powers[p], anchor_x, anchor_y - offset, this->fgcolors[G::Power]);
		}

		for (GMeter g = _E0(GMeter); g < GMeter::_; g++) {
			CanvasTextLayout^ upper_target = ((g == GMeter::sea) ? this->foil_filter_pdrop : this->temperatures[g]);
			offset = upper_target->LayoutBounds.Width * 0.5F;
			this->fill_gauges_anchor(idx, g, 0.26F, &anchor_x, &anchor_y);
			ds->DrawTextLayout(upper_target, anchor_x - offset, anchor_y, this->fgcolors[G::Gauge]);

			offset = this->pressures[g]->LayoutBounds.Width * 0.5F;
			this->fill_gauges_anchor(idx, g, 0.76F, &anchor_x, &anchor_y);
			ds->DrawTextLayout(this->pressures[g], anchor_x - offset, anchor_y, this->fgcolors[G::Gauge]);
		}
	}

private:
	CanvasTextLayout^ rspeed;
	CanvasTextLayout^ foil_filter_pdrop;
	std::map<GPower, CanvasTextLayout^> powers;
	std::map<GMeter, CanvasTextLayout^> temperatures;
	std::map<GMeter, CanvasTextLayout^> pressures;

private:
	ICanvasBrush^ rspeed_bgcolors[gcount];
	ICanvasBrush^ power_cell_color;
	std::map<G, ICanvasBrush^> fgcolors;
	std::map<G, ICanvasBrush^> bgcolors;

private:
	float region_width;
	float region_height;
	float region_padding;
	std::map<G, float> ys;
	std::map<G, float> heights;

private:
	IPlanet* master;
};

/*************************************************************************************************/
private class GBoard final : public PLCConfirmation {
public:
	~GBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	GBoard(GeneratorPage* master, GDecorator* decorator) : master(master), decorator(decorator) {
		Platform::String^ scale_face = "Arial";
		this->rspeed_fonts[0] = make_text_format(scale_face, this->master->sketch_to_application_height(125.0F));
		this->rspeed_fonts[1] = make_text_format(scale_face, this->master->sketch_to_application_height(45.00F));
		this->power_fonts[0] = make_text_format(scale_face, this->master->sketch_to_application_height(42.0F));
		this->power_fonts[1] = make_text_format(scale_face, this->master->sketch_to_application_height(37.5F));
		this->gauge_fonts[0] = make_text_format(scale_face, this->master->sketch_to_application_height(32.0F));
		this->gauge_fonts[1] = make_text_format(scale_face, this->master->sketch_to_application_height(28.00F));

		this->fgcolor = Colours::make(0xD2D2D2);

		this->decorator->reference();
	}

public:
	void load_and_flow() {
		float anchor_x, anchor_y, gauge_size;

		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->decorator->fill_rspeed_anchor(idx, 0.64F, 0.5F, &anchor_x, &anchor_y);
			this->rspeeds[idx] = new Dimensionlet("<rpm>", this->rspeed_fonts[0], this->rspeed_fonts[1], this->fgcolor);
			this->master->insert(this->rspeeds[idx], anchor_x, anchor_y, GraphletAnchor::CC);

			for (GPower p = _E0(GPower); p < GPower::_; p++) {
				Platform::String^ unit = "<" + p.ToString() + ">";

				this->decorator->fill_power_anchor(idx, p, 0.9F, 0.75F, &anchor_x, &anchor_y);
				this->powers[p][idx] = new Dimensionlet(unit, this->power_fonts[0], this->power_fonts[1], this->fgcolor);
				this->master->insert(this->powers[p][idx], anchor_x, anchor_y, GraphletAnchor::RC);
			}

			for (GMeter m = _E0(GMeter); m < GMeter::_; m++) {
				{ // load upper indicators
					this->decorator->fill_gauges_anchor(idx, m, 0.25F, &anchor_x, &anchor_y, &gauge_size);
					
					if (m == GMeter::sea) {
						this->foil_filter_pdmeter[idx] = new Indicatorlet(gauge_size, indicator_thickness);
						this->foil_filter_pdrop[idx] = new Dimensionlet("<pdrop>", this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);
						
						this->master->insert(this->foil_filter_pdmeter[idx], anchor_x, anchor_y, GraphletAnchor::CC);
						this->master->insert(this->foil_filter_pdrop[idx], anchor_x, anchor_y, GraphletAnchor::CB);
					} else {
						this->thermometers[m][idx] = new Indicatorlet(gauge_size, indicator_thickness);
						this->temperatures[m][idx] = new Dimensionlet("<temperature>", this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);

						this->master->insert(this->thermometers[m][idx], anchor_x, anchor_y, GraphletAnchor::CC);
						this->master->insert(this->temperatures[m][idx], anchor_x, anchor_y, GraphletAnchor::CB);
					}
				}

				{ // load bottom indicators
					this->manometers[m][idx] = new Indicatorlet(gauge_size, indicator_thickness);
					this->pressures[m][idx] = new Dimensionlet("<pressure>", this->gauge_fonts[0], this->gauge_fonts[1], this->fgcolor);

					this->decorator->fill_gauges_anchor(idx, m, 0.75F, &anchor_x, &anchor_y, &gauge_size);
					this->master->insert(this->manometers[m][idx], anchor_x, anchor_y, GraphletAnchor::CC);
					this->master->insert(this->pressures[m][idx], anchor_x, anchor_y, GraphletAnchor::CB);
				}
			}
		}
	}

public:
	void on_analog_input_data(uint8* db4, size_t size, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		{ // metrics in PMS section
			size_t db_idx_acc = 11;

			this->set_values(this->powers[GPower::frequency], db4, 3U, db_idx_acc, GraphletAnchor::RB);
			this->set_values(this->rspeeds,                   db4, 4U, db_idx_acc, GraphletAnchor::RB);
			this->set_values(this->powers[GPower::current],   db4, 5U, db_idx_acc, GraphletAnchor::RB);
			this->set_values(this->powers[GPower::voltage],   db4, 6U, db_idx_acc, GraphletAnchor::RB);
		}

		{ // metrics in generator section
			size_t db_idx_acc = 10;

			this->set_temperature_meter(GMeter::coolant, db4, 76U, db_idx_acc);
			this->set_pressure_meter(   GMeter::oil,     db4, 77U, db_idx_acc);
			this->set_temperature_meter(GMeter::oil,     db4, 78U, db_idx_acc);
			this->set_pressure_meter(   GMeter::sea,     db4, 80U, db_idx_acc);
			this->set_pressure_meter(   GMeter::coolant, db4, 84U, db_idx_acc);

			this->set_meters(this->foil_filter_pdrop, this->foil_filter_pdmeter,
				db4, 83U, db_idx_acc, GraphletAnchor::CC);
		}

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	void set_meters(Dimensionlet* dims[], Indicatorlet* idts[], uint8* db, size_t idx0, size_t acc, GraphletAnchor anchor = GraphletAnchor::CC) {
		for (unsigned int idx = 0; idx < gcount; idx++) {
			float v = AI_ref(db, idx0 + acc * idx);

			dims[idx]->set_value(v, anchor);
			idts[idx]->set_value(v);
		}
	}

	void set_pressure_meter(GMeter m, uint8* db, size_t idx0, size_t acc) {
		this->set_meters(this->pressures[m], this->manometers[m], db, idx0, acc);
	}

	void set_temperature_meter(GMeter m, uint8* db, size_t idx0, size_t acc) {
		this->set_meters(this->temperatures[m], this->thermometers[m], db, idx0, acc);
	}

private:
	template<class G>
	void set_values(G* gs[], uint8* db, size_t idx0, size_t acc) {
		for (size_t idx = 0; idx < gcount; idx++) {
			gs[idx]->set_value(AI_ref(db, idx0 + acc * idx));
		}
	}

	template<class G>
	void set_values(G* gs[], uint8* db, size_t idx0, size_t acc, GraphletAnchor a) {
		for (size_t idx = 0; idx < gcount; idx++) {
			gs[idx]->set_value(AI_ref(db, idx0 + acc * idx), a);
		}
	}

// never deletes these graphlets mannually
private:
	Dimensionlet* rspeeds[gcount];
	Dimensionlet* foil_filter_pdrop[gcount];
	Indicatorlet* foil_filter_pdmeter[gcount];
	std::map<GPower, Dimensionlet*[gcount]> powers;
	std::map<GMeter, Dimensionlet*[gcount]> pressures;
	std::map<GMeter, Dimensionlet*[gcount]> temperatures;
	std::map<GMeter, Indicatorlet*[gcount]> manometers;
	std::map<GMeter, Indicatorlet*[gcount]> thermometers;
		
private:
	CanvasTextFormat^ rspeed_fonts[2];
	CanvasTextFormat^ power_fonts[2];
	CanvasTextFormat^ gauge_fonts[2];
	ICanvasBrush^ fgcolor;
	GeneratorPage* master;
	GDecorator* decorator;
};

/*************************************************************************************************/
GeneratorPage::GeneratorPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

GeneratorPage::~GeneratorPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void GeneratorPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		GDecorator* regions = new GDecorator(this, width, height, this->sketch_to_application_height(4.0F));
		GBoard* gb = new GBoard(this, regions);

		gb->load_and_flow();

		this->dashboard = gb;
		this->set_decorator(regions);
		this->device->append_confirmation_receiver(gb);
	}
}

void GeneratorPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
