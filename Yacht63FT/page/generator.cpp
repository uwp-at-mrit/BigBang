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

private enum class GInfo { mode, t_sea, t_pipe, aux, _ };
private enum class GMode { Breakdown, Heating, Refrigeration, _ };
private enum class GStatus { Normal };

static const unsigned int region_count = 2U;

static const unsigned int decorator_text_color = 0x666666U;

private class GDecorator final : public IPlanetDecorator {
public:
	GDecorator(float width, float height, float padding) : region_height(height), region_padding(padding) {
		this->region_width = (width - padding) / float(region_count) - padding;
		
		this->bgcolor = Colours::make(decorator_text_color);
		this->rpm_bgcolors[0] = Colours::make(0x101410U);
		this->rpm_bgcolors[1] = Colours::make(0x151915U);
		this->power_cell_color = Colours::make(0x131615U);
		this->power_bgcolor = Colours::make(0x313131U);
		this->gauges_bgcolor = Colours::make(0x1E1E1EU);
	}

public:
	void draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int i = 0; i < region_count; i++) {
			this->draw_region(ds, i);
		}
	}

public:
	void fill_info_anchor(unsigned int idx, GInfo id, float* anchor_x, float* anchor_y) {
		float x, y, width, height;

		switch (id) {
		case GInfo::mode:   SET_VALUES(anchor_x, x + width * 0.25F, anchor_y, y + height * 0.64F); break;
		case GInfo::t_sea:  SET_VALUES(anchor_x, x + width * 0.75F, anchor_y, y + height * 0.64F); break;
		case GInfo::t_pipe: SET_VALUES(anchor_x, x + width * 0.25F, anchor_y, y + height * 0.86F); break;
		case GInfo::aux:    SET_VALUES(anchor_x, x + width * 0.75F, anchor_y, y + height * 0.86F); break;
		}
	}

private:
	void draw_region(CanvasDrawingSession^ ds, unsigned int idx) {
		float x = (this->region_width + this->region_padding) * float(idx) + this->region_padding;
		float rpm_height = design_to_application_height(182.0F);
		float power_y = this->region_padding + rpm_height;
		float power_height = design_to_application_height(125.0F);
		float gauges_y = power_y + power_height + this->region_padding;
		float gauges_height = design_to_application_height(514.0F);

		ds->FillRectangle(x, this->region_padding, this->region_width, rpm_height, this->rpm_bgcolors[idx]);
		ds->FillRectangle(x, gauges_y, this->region_width, gauges_height, this->gauges_bgcolor);
		
		{ // draw power subregions
			float cell_gapsize = design_to_application_width(8.0F);
			float cell_height = design_to_application_height(102.0F);
			float cell_margin = (power_height - cell_height) * 0.5F;
			float cell_width = (this->region_width - cell_margin * 2.0F - cell_gapsize * 2.0F) / 3.0F;
			float cell_x = x + cell_margin;
			float cell_y = power_y + cell_margin;

			ds->FillRectangle(x, power_y, this->region_width, power_height, this->power_bgcolor);
			for (unsigned int i = 0; i < 3; i++) {
				ds->FillRoundedRectangle(
					cell_x + (cell_width + cell_gapsize) * float(i), cell_y,
					cell_width, cell_height, cell_gapsize, cell_gapsize,
					this->power_cell_color);
			}
		}
	}

private:
	ICanvasBrush^ bgcolor;
	ICanvasBrush^ rpm_bgcolors[region_count];
	ICanvasBrush^ power_cell_color;
	ICanvasBrush^ power_bgcolor;
	ICanvasBrush^ gauges_bgcolor;

private:
	float region_width;
	float region_height;
	float region_padding;
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

private:
	void load_info(IGraphlet* g, unsigned int i, GInfo type) {
		float anchor_x, anchor_y;

		this->decorator->fill_info_anchor(i, type, &anchor_x, &anchor_y);
		this->master->insert(g, anchor_x, anchor_y, GraphletAlignment::CB);
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
