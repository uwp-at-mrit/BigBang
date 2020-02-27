#include <map>

#include "graphlet/ui/metricslet.hpp"

#include "graphlet/ui/textlet.hpp"
#include "graphlet/shapelet.hpp"

#include "filesystem/msappdata.hxx"

#include "datum/credit.hpp"
#include "datum/string.hpp"

#include "module.hpp"
#include "brushes.hxx"
#include "planet.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
static unsigned int default_slot_count = 20U;

static CanvasTextFormat^ default_metrics_label_font = make_bold_text_format("Microsoft YaHei", 18.0F);
static CanvasTextFormat^ default_metrics_font = make_bold_text_format("Cambria Math", 20.0F);

#define SET_METRICS(ms, id, v, p) \
do { \
    auto t = ms.find(id); \
    if (t != ms.end()) { \
        t->second->set_text(flstring(v, p), GraphletAnchor::RC); \
    } \
} while(0)

#define SET_VALID_METRICS(ms, id, b, v, p) \
do { \
    auto t = ms.find(id); \
    if (t != ms.end()) { \
        t->second->set_text((b ? flstring(v, p) : "-"), GraphletAnchor::RC); \
    } \
} while(0)

/*************************************************************************************************/
static void prepare_metrics_style(MetricsStyle* style) {
	CAS_SLOT(style->label_font, default_metrics_label_font);
	CAS_SLOT(style->metrics_font, default_metrics_font);

	CAS_SLOT(style->background, Colours::Background);
	CAS_SLOT(style->slot_color, Colours::Background);
	CAS_SLOT(style->slot_border, Colours::DarkGray);
}

MetricsStyle WarGrey::DTPM::make_metrics_style(CanvasTextFormat^ lblft, CanvasTextFormat^ mft, CanvasSolidColorBrush ^ bg, CanvasSolidColorBrush ^ slot_bg) {
	MetricsStyle s;

	s.label_font = lblft;
	s.metrics_font = mft;

	s.background = bg;
	s.slot_color = slot_bg;

	return s;
}

/*************************************************************************************************/
namespace {
	private ref class Metrics sealed {
	public:
		static Metrics^ load(Platform::String^ path) {
			return nullptr;
		}
		
		static bool save(Metrics^ self, Platform::String^ path) {
			return true;
		}

	public:
		Metrics(Metrics^ src = nullptr) {

		}

	public:
		void refresh(Metrics^ src) {

		}
	};

	private class MetricsFrame final : public virtual Planet, public virtual IMsAppdata<Metrics> {
	public:
		MetricsFrame(Metricslet* master, Platform::String^ name, MetricsStyle& style, unsigned int slots)
			: Planet(name), master(master), style(style), slot_count(slots > 0 ? slots : default_slot_count) {
			this->ms_appdata_config = ms_appdata_file(name, ".config", "configuration/metrics");

			prepare_metrics_style(&this->style);
			this->slot_height = this->style.metrics_font->FontSize * 1.2F;
			this->inset = this->style.metrics_font->FontSize * 0.618F;
			this->hgapsize = this->inset * 0.618F;
		}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override {
			float corner_radius = 8.0F;
			float slot_width = width - this->inset * 2.0F;
			float bgheight = (this->slot_height + this->hgapsize) * float(this->slot_count) + this->inset * 2.0F;

			IMsAppdata::load(this->ms_appdata_config, "metrics style");
			this->background = this->insert_one(new Rectanglet(width, bgheight, this->style.background));

			//for (unsigned int idx = 0; idx < this->slot_count; idx++) {
			//	M id = _E(M, idx);
			//	ICanvasBrush^ color = matrics_color_map(id);

			//	this->slots[id] = this->master->insert_one(new Credit<RoundedRectanglet, M>(
			//		slot_width, this->slot_height, corner_radius, metrics_background), id);

			//	this->labels[id] = this->master->insert_one(new Credit<Labellet, M>(_speak(id), this->label_font, color), id);
			//	this->metrics[id] = this->master->insert_one(new Credit<Labellet, M>("0.0", this->metrics_font, color));
			//}
		}

		void reflow(float width, float height) override {
			//for (unsigned int idx = 0; idx < this->slot_count; idx++) {
			//	M id = _E(M, idx);

			//	this->master->move_to(this->slots[id], this->inset, this->inset + (this->slot_height + this->hgapsize) * idx);
				
			//	this->master->move_to(this->labels[id], this->slots[id], GraphletAnchor::LC, GraphletAnchor::LC, this->hgapsize);
			//	this->master->move_to(this->metrics[id], this->slots[id], GraphletAnchor::RC, GraphletAnchor::RC, -this->hgapsize);
			//}
		}

	protected:
		void on_appdata(Windows::Foundation::Uri^ ms_appdata, Metrics^ ftobject) override {

		}

		void on_appdata_not_found(Windows::Foundation::Uri^ ms_appdata) override {
			IMsAppdata::write(this->metrics_config, this->ms_appdata_config->Path);
			this->log_message(Log::Error,
				make_wstring(L"failed to load %s: file does not exist",
					ms_appdata->ToString()->Data()));
		}

		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->master->get_logger()->log_message(level, message);
		}

	private: // never deletes these graphlets manually
		//std::map<M, Credit<RoundedRectanglet, M>*> slots;
		//std::map<M, Credit<Labellet, M>*> labels;
		//std::map<M, Credit<Labellet, M>*> metrics;
		Rectanglet* background;

	private:
		Uri^ ms_appdata_config;
		Metrics^ metrics_config;
		MetricsStyle style;

	private:
		unsigned int slot_count;
		float slot_height;
		float hgapsize;
		float inset;

	private: // never deletes these objects manually
		Metricslet* master;
	};
}

/*************************************************************************************************/
Metricslet::Metricslet(Platform::String^ name, float width, GraphletAnchor anchor, unsigned int slot_count)
	: Metricslet(make_metrics_style(), name, width, anchor, slot_count) {}

Metricslet::Metricslet(MetricsStyle& style, Platform::String^ name, float width, GraphletAnchor anchor, unsigned int slot_count)
	: Planetlet(new MetricsFrame(this, name, style, slot_count), width) {
	this->set_stretch_anchor(anchor);
	this->enable_events(true, true);
}
