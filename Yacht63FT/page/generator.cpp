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
	GDecorator(IPlanet* master, float padding) : master(master), region_padding(padding) {
		this->region_width = (1.0F - padding) / float(gcount) - padding;

		this->rspeed_bgcolors[0] = Colours::make(0x101410U);
		this->rspeed_bgcolors[1] = Colours::make(0x151915U);
		this->power_cell_color = Colours::make(0x131615U);
		this->fgcolors[G::RSpeed] = Colours::GhostWhite;
		this->fgcolors[G::Power] = Colours::make(0x878787U);
		this->fgcolors[G::Gauge] = Colours::make(0x919191U);
		this->bgcolors[G::Power] = Colours::make(0x313131U);
		this->bgcolors[G::Gauge] = Colours::make(0x1E1E1EU);
		this->bgcolors[G::Alert] = Colours::make(0x131615U);

		this->heights[G::RSpeed] = 180.0F / sketch_height;
		this->heights[G::Power] = 125.0F / sketch_height;
		this->heights[G::Alert] = 70.0F / sketch_height;
		this->heights[G::Gauge] = 1.0F
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
				this->powers[p] = make_text_layout(speak(":" + p.ToString() + ":"), pfont);
			}

			for (GMeter g = _E0(GMeter); g < GMeter::_; g++) {
				if (g == GMeter::sea) {
					this->foil_filter_pdrop = make_text_layout(speak(":pd_filter:"), mfont);
				} else {
					this->temperatures[g] = make_text_layout(speak(g, "temperature"), mfont);
				}

				this->pressures[g] = make_text_layout(speak(g, "pressure"), mfont);
			}
		}
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->draw_region(ds, idx);
		}
	}

	void draw_after(CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->draw_region_label(ds, idx);
		}
	}

public:
	void fill_power_cell_extent(unsigned int g_idx, GPower p, float* x, float* y, float* width, float* height) {
		static float cell_gapsize = 8.0F / sketch_width;
		static float cell_height = 102.0F / sketch_height;
		static float cell_margin = (this->heights[G::Power] - cell_height) * 0.5F;
		static float cell_width = (this->region_width - cell_margin * 2.0F - cell_gapsize * 2.0F) / 3.0F;

		float awidth = this->actual_width();
		float aheight = this->actual_height();
		float left_x = (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
		float cell_x = left_x + cell_margin + (cell_width + cell_gapsize) * _F(p);
		float cell_y = this->ys[G::Power] + cell_margin;

		SET_VALUES(x, cell_x * awidth, y, cell_y * aheight);
		SET_VALUES(width, cell_width * awidth, height, cell_height * aheight);
	}

	void fill_rspeed_anchor(unsigned int g_idx, float fw, float fh, float* x, float* y) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		float anchor_x = this->region_x(g_idx) + this->region_width * fw;
		float anchor_y = this->ys[G::RSpeed] + this->heights[G::RSpeed] * fh;

		SET_VALUES(x, anchor_x * awidth, y, anchor_y * aheight);
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
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		float gauge_x = this->region_x(g_idx) +  subwidth * (_F(g) + 0.5F);
		float gauge_y = this->ys[G::Gauge] + this->heights[G::Gauge] * fh;

		SET_VALUES(x, gauge_x * awidth, y, gauge_y * aheight);

		if (cell_size != nullptr) {
			(*cell_size) = fmin(subwidth * awidth, this->heights[G::Gauge] * aheight * 0.5F) * 0.9F;
		}
	}

private:
	float region_x(unsigned int g_idx) {
		return (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
	}

	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		float x = this->region_x(idx) * awidth;
		float rwidth = this->region_width * awidth;

		ds->FillRectangle(x, this->ys[G::RSpeed] * aheight,
			rwidth, this->heights[G::RSpeed] * aheight,
			this->rspeed_bgcolors[idx]);

		for (G region = _E(G, 1); region < G::_; region++) {
			float y = this->ys[region] * aheight;
			float height = this->heights[region] * aheight;
			ICanvasBrush^ color = this->bgcolors[region];

			if (region != G::Alert) {
				ds->FillRectangle(x, y, rwidth, height, color);
			} else {
				ds->FillRoundedRectangle(x, y, rwidth, height, corner_radius, corner_radius, color);
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
	GBoard(GeneratorPage* master, GDecorator* decorator) : master(master), decorator(decorator) {
		Platform::String^ scale_face = "Arial";
		auto fgcolor = Colours::make(0xD2D2D2);

		this->rspeed_style.number_font = make_text_format(scale_face, this->master->sketch_to_application_height(125.0F));
		this->rspeed_style.unit_font = make_text_format(scale_face, this->master->sketch_to_application_height(45.00F));
		this->rspeed_style.number_color = fgcolor;
		this->rspeed_style.unit_color = fgcolor;

		this->power_style.number_font = make_text_format(scale_face, this->master->sketch_to_application_height(42.0F));
		this->power_style.unit_font = make_text_format(scale_face, this->master->sketch_to_application_height(37.5F));
		this->power_style.number_color = fgcolor;
		this->power_style.unit_color = fgcolor;

		this->gauge_style.number_font = make_text_format(scale_face, this->master->sketch_to_application_height(32.0F));
		this->gauge_style.unit_font = make_text_format(scale_face, this->master->sketch_to_application_height(28.00F));
		this->gauge_style.number_color = fgcolor;
		this->gauge_style.unit_color = fgcolor;
	}

public:
	void load() {
		float gauge_size;

		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->rspeeds[idx] = this->master->insert_one(new Dimensionlet(this->rspeed_style, "<rpm>"));

			for (GPower p = _E0(GPower); p < GPower::_; p++) {
				Platform::String^ unit = "<" + p.ToString() + ">";

				this->powers[p][idx] = this->master->insert_one(new Dimensionlet(this->power_style, unit));
			}

			for (GMeter m = _E0(GMeter); m < GMeter::_; m++) {
				this->decorator->fill_gauges_anchor(idx, m, 0.25F, nullptr, nullptr, &gauge_size);

				{ // load upper indicators
					if (m == GMeter::sea) {
						this->foil_filter_pdmeter[idx] = this->master->insert_one(new Indicatorlet(gauge_size, indicator_thickness));
						this->foil_filter_pdrop[idx] = this->master->insert_one(new Dimensionlet(this->gauge_style, "<pdrop>"));
					} else {
						this->thermometers[m][idx] = this->master->insert_one(new Indicatorlet(gauge_size, indicator_thickness));
						this->temperatures[m][idx] = this->master->insert_one(new Dimensionlet(this->gauge_style, "<temperature>"));
					}
				}

				{ // load bottom indicators
					this->manometers[m][idx] = this->master->insert_one(new Indicatorlet(gauge_size, indicator_thickness));
					this->pressures[m][idx] = this->master->insert_one(new Dimensionlet(this->gauge_style, "<pressure>"));
				}
			}
		}
	}

	void reflow() {
		float anchor_x, anchor_y;

		for (unsigned int idx = 0; idx < gcount; idx++) {
			this->decorator->fill_rspeed_anchor(idx, 0.64F, 0.5F, &anchor_x, &anchor_y);
			this->master->move_to(this->rspeeds[idx], anchor_x, anchor_y, GraphletAnchor::CC);

			for (GPower p = _E0(GPower); p < GPower::_; p++) {
				Platform::String^ unit = "<" + p.ToString() + ">";

				this->decorator->fill_power_anchor(idx, p, 0.9F, 0.75F, &anchor_x, &anchor_y);
				this->master->move_to(this->powers[p][idx], anchor_x, anchor_y, GraphletAnchor::RC);
			}

			for (GMeter m = _E0(GMeter); m < GMeter::_; m++) {
				{ // load upper indicators
					this->decorator->fill_gauges_anchor(idx, m, 0.25F, &anchor_x, &anchor_y, nullptr);

					if (m == GMeter::sea) {
						this->master->move_to(this->foil_filter_pdmeter[idx], anchor_x, anchor_y, GraphletAnchor::CC);
						this->master->move_to(this->foil_filter_pdrop[idx], anchor_x, anchor_y, GraphletAnchor::CB);
					} else {
						this->master->move_to(this->thermometers[m][idx], anchor_x, anchor_y, GraphletAnchor::CC);
						this->master->move_to(this->temperatures[m][idx], anchor_x, anchor_y, GraphletAnchor::CB);
					}
				}

				{ // load bottom indicators
					this->decorator->fill_gauges_anchor(idx, m, 0.75F, &anchor_x, &anchor_y, nullptr);
					this->master->move_to(this->manometers[m][idx], anchor_x, anchor_y, GraphletAnchor::CC);
					this->master->move_to(this->pressures[m][idx], anchor_x, anchor_y, GraphletAnchor::CB);
				}
			}
		}
	}

public:
	void on_analog_input(uint8* db4, size_t size, Syslog* logger) override {
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
	DimensionStyle rspeed_style;
	DimensionStyle power_style;
	DimensionStyle gauge_style;
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
		GDecorator* regions = new GDecorator(this, 4.0F / sketch_height);
		GBoard* gb = new GBoard(this, regions);

		this->append_decorator(regions);
		gb->load();

		this->dashboard = gb;
		this->device->append_confirmation_receiver(gb);
	}
}

void GeneratorPage::reflow(float width, float height) {
	GBoard* gb = dynamic_cast<GBoard*>(this->dashboard);

	if (gb != nullptr) {
		gb->reflow();
	}
}

void GeneratorPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
