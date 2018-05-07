#include "page/generator.hpp"
#include "decorator/decorator.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/thermometerlet.hpp"
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

private enum class G { RPM, Power, Gauges, Alert, _ };
private enum class GStatus { Normal };

static const unsigned int region_count = 2U;

static const unsigned int decorator_text_color = 0x666666U;

private class GDecorator final : public IPlanetDecorator {
public:
	GDecorator(float width, float height, float padding) : region_height(height), region_padding(padding) {
		this->region_width = (width - padding) / float(region_count) - padding;

		this->rpm_bgcolors[0] = Colours::make(0x101410U);
		this->rpm_bgcolors[1] = Colours::make(0x151915U);
		this->power_cell_color = Colours::make(0x131615U);
		this->fgcolors[G::Power] = Colours::make(0x878787U);
		this->fgcolors[G::Gauges] = Colours::make(0x919191U);
		this->bgcolors[G::Power] = Colours::make(0x313131U);
		this->bgcolors[G::Gauges] = Colours::make(0x1E1E1EU);
		this->bgcolors[G::Alert] = Colours::make(0x131615U);

		this->heights[G::RPM] = design_to_application_height(180.0F);
		this->heights[G::Power] = design_to_application_height(125.0F);
		this->heights[G::Alert] = design_to_application_height(70.0F);
		this->heights[G::Gauges] = height
			- (this->heights[G::RPM] + this->heights[G::Power] + this->heights[G::Alert])
			- this->region_padding * float(static_cast<unsigned int>(G::_) + 1);

		this->ys[G::RPM] = this->region_padding;
		for (unsigned int region = 1; region < static_cast<unsigned int>(G::_); region++) {
			G prev = static_cast<G>(region - 1);

			this->ys[static_cast<G>(region)] = this->ys[prev] + this->heights[prev] + this->region_padding;
		}

		{ // initialize labels
			CanvasTextFormat^ rpm_font = make_text_format("Microsoft YaHei", design_to_application_height(33.75F));
			CanvasTextFormat^ power_font = make_text_format("Microsoft YaHei", design_to_application_height(30.0F));

			this->rpm = make_text_layout(speak("rpm"), rpm_font);
			this->voltage = make_text_layout(speak("voltage"), power_font);
			this->ampere = make_text_layout(speak("ampere"), power_font);
			this->frequency = make_text_layout(speak("frequncy"), power_font);
		}
	}

public:
	void draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int i = 0; i < region_count; i++) {
			this->draw_region(ds, i);
		}
	}

public:
	void fill_power_cell_extent(unsigned int g_idx, unsigned int c_idx, float* x, float* y, float* width, float* height) {
		static float cell_gapsize = design_to_application_width(8.0F);
		static float cell_height = design_to_application_height(102.0F);
		static float cell_margin = (this->heights[G::Power] - cell_height) * 0.5F;
		static float cell_width = (this->region_width - cell_margin * 2.0F - cell_gapsize * 2.0F) / 3.0F;

		float left_x = (this->region_width + this->region_padding) * float(g_idx) + this->region_padding;
		float cell_x = left_x + cell_margin + (cell_width + cell_gapsize) * float(c_idx);
		float cell_y = this->ys[G::Power] + cell_margin;

		SET_VALUES(x, cell_x, y, cell_y);
		SET_VALUES(width, cell_width, height, cell_height);
	}

private:
	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float x = (this->region_width + this->region_padding) * float(idx) + this->region_padding;

		ds->FillRectangle(x, this->ys[G::RPM], this->region_width, this->heights[G::RPM], this->rpm_bgcolors[idx]);

		for (G region = static_cast<G>(1); region < G::_; region++) {
			float y = this->ys[region];
			float height = this->heights[region];

			if (region != G::Alert) {
				ds->FillRectangle(x, y, this->region_width, height, this->bgcolors[region]);
			} else {
				ds->FillRoundedRectangle(x, y, this->region_width, height, 8.0F, 8.0F, this->bgcolors[region]);
			}
		}

		{ // draw power subregions
			float cell_x, cell_y, cell_width, cell_height;

			for (unsigned int i = 0; i < 3; i++) {
				this->fill_power_cell_extent(idx, i, &cell_x, &cell_y, &cell_width, &cell_height);
				ds->FillRoundedRectangle(cell_x, cell_y, cell_width, cell_height, 8.0F, 8.0F, this->power_cell_color);
			}
		}
	}

private:
	CanvasTextLayout^ rpm;
	CanvasTextLayout^ voltage;
	CanvasTextLayout^ ampere;
	CanvasTextLayout^ frequency;

private:
	ICanvasBrush^ rpm_bgcolors[region_count];
	ICanvasBrush^ power_cell_color;
	std::map<G, ICanvasBrush^> fgcolors;
	std::map<G, ICanvasBrush^> bgcolors;

private:
	float region_width;
	float region_height;
	float region_padding;
	std::map<G, float> ys;
	std::map<G, float> heights;
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
		this->fonts[0] = make_text_format("Microsoft YaHei", screen_to_application_size(33.75F));
		this->fonts[1] = make_text_format("Microsoft YaHei", screen_to_application_size(37.50F));
		this->fonts[2] = make_text_format("Microsoft YaHei", screen_to_application_size(30.00F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		Platform::String^ T = speak("celsius");
		float cell_x, cell_y, cell_width, cell_height, cell_whalf, label_bottom;
		float label_yoffset = screen_to_application_size(screen_caption_yoff);
		float icon_width = screen_to_application_size(64.0F);
		float mode_width = screen_to_application_size(46.0F);
	}

// never deletes these graphlets mannually
private:
		
private:
	CanvasTextFormat^ fonts[3];
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
		GDecorator* regions = new GDecorator(width, height, design_to_application_height(2.0F));
		GBoard* gb = new GBoard(this, regions);

		//gb->load_and_flow(width, height);

		this->dashboard = gb;
		this->set_decorator(regions);
		this->device->append_confirmation_receiver(gb);
	}
}

void GeneratorPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
