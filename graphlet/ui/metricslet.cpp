#include <map>
#include <vector>

#include "graphlet/ui/metricslet.hpp"

#include "graphlet/ui/textlet.hpp"
#include "graphlet/shapelet.hpp"

#include "filesystem/msappdata.hxx"

#include "datum/credit.hpp"
#include "datum/string.hpp"
#include "datum/fixnum.hpp"
#include "datum/file.hpp"

#include "module.hpp"
#include "brushes.hxx"
#include "planet.hpp"

#include "colorspace.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
static Platform::String^ metrics_file_type = "metrics style";
static Platform::String^ metrics_tongue = "status";

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

std::map<unsigned int, CanvasSolidColorBrush^> colors;

static CanvasSolidColorBrush^ color_ref(unsigned int hex) {
	auto maybe_color = colors.find(hex);

	if (maybe_color == colors.end()) {
		colors[hex] = Colours::make(hex);
	}

	return colors[hex];
}

/*************************************************************************************************/
static void prepare_metrics_style(MetricsStyle* style) {
	CAS_SLOT(style->label_font, default_metrics_label_font);
	CAS_SLOT(style->metrics_font, default_metrics_font);

	CAS_SLOT(style->border_color, Colours::DimGray);
	CAS_SLOT(style->background_color, Colours::Background);
	CAS_SLOT(style->slot_color_color, Colours::Background);
	CAS_SLOT(style->slot_border_color, Colours::Transparent);

	CAS_SLOT(style->nodatum_color, Colours::DimGray);

	FLCAS_SLOT(style->border_thickness, 2.0F);
}

MetricsStyle WarGrey::DTPM::make_metrics_style(CanvasTextFormat^ lblft, CanvasTextFormat^ mft, CanvasSolidColorBrush ^ bg, CanvasSolidColorBrush ^ slot_bg) {
	MetricsStyle s;

	s.label_font = lblft;
	s.metrics_font = mft;

	s.background_color = bg;
	s.slot_color_color = slot_bg;

	return s;
}

/*************************************************************************************************/
namespace {
	private struct Metric {
		unsigned int uuid;
		unsigned int label_color;
		unsigned int value_color;
	};

	private ref class Metrics sealed {
	public:
		static Metrics^ load(Platform::String^ path) {
			return nullptr;
		}
		
		static bool save(Metrics^ self, Platform::String^ path) {
			std::wofstream m_config;
			bool okay = false;

			if (open_output_binary(m_config, path)) {
				for (auto m = self->db.begin(); m != self->db.end(); m++) {
					m_config << " " << m->uuid << " " << m->label_color << " " << m->value_color;
					write_newline(m_config);
				}
			}

			return okay;
		}

	public:
		Metrics(Metrics^ src = nullptr) {

		}

	public:
		void refresh(Metrics^ src) {

		}

	internal:
		Metrics(Metricslet* master, size_t slot_count) {
			unsigned int total = fxmin((unsigned int)slot_count, master->capacity());

			for (unsigned int idx = 0; idx < total; idx++) {
				Metric m;

				m.uuid = idx;
				m.label_color = color_to_hexadecimal(master->label_color_ref(idx)->Color);
				m.value_color = color_to_hexadecimal(master->label_color_ref(idx)->Color);

				this->db.push_back(m);
			}
		}

	internal:
		std::vector<Metric> db;
	};

	private class MetricsFrame final : public virtual Planet, public virtual IMsAppdata<Metrics> {
	public:
		MetricsFrame(Metricslet* master, Platform::String^ name, MetricsStyle& style, size_t slots)
			: Planet(name), master(master), style(style), slot_count(slots > 0 ? slots : default_slot_count), metrics_config(nullptr) {
			this->ms_appdata_config = ms_appdata_file(name, ".config", "metrics");

			prepare_metrics_style(&this->style);
			this->slot_height = this->style.metrics_font->FontSize * 1.2F;
			this->inset = this->style.metrics_font->FontSize * 0.618F;
			this->hgapsize = this->inset * 0.618F;

			this->slots = new Credit<RoundedRectanglet, size_t>*[this->slot_count];
			this->labels = new Credit<Labellet, size_t>*[this->slot_count];
			this->metrics = new Credit<Labellet, size_t>*[this->slot_count];
		}

		~MetricsFrame() noexcept {
			if (this->slots != nullptr) {
				delete[] this->slots;
			}

			if (this->labels != nullptr) {
				delete[] this->labels;
			}

			if (this->metrics != nullptr) {
				delete[] this->metrics;
			}
		}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override {
			float corner_radius = 8.0F;
			float slot_width = width - this->inset * 2.0F;
			float bgheight = (this->slot_height + this->hgapsize) * float(this->slot_count) + this->inset * 2.0F;

			this->background = this->insert_one(new Rectanglet(width - this->style.border_thickness, bgheight,
				this->style.background_color, this->style.border_color, this->style.border_thickness));

			for (size_t idx = 0; idx < this->slot_count; idx++) {
				ICanvasBrush^ color = Colours::Foreground;

				this->slots[idx] = this->insert_one(new Credit<RoundedRectanglet, size_t>(
					slot_width, this->slot_height, corner_radius, this->style.slot_color_color, this->style.slot_border_color), idx);

				this->labels[idx] = this->insert_one(new Credit<Labellet, size_t>(speak("loading", metrics_tongue), this->style.label_font, color), idx);
				this->metrics[idx] = this->insert_one(new Credit<Labellet, size_t>("0.0", this->style.metrics_font, color), idx);
			}

			IMsAppdata::load(this->ms_appdata_config, metrics_file_type);
		}

		void reflow(float width, float height) override {
			for (size_t idx = 0; idx < this->slot_count; idx++) {
				this->move_to(this->slots[idx], this->inset, this->inset + (this->slot_height + this->hgapsize) * idx);
				
				this->move_to(this->labels[idx], this->slots[idx], GraphletAnchor::LC, GraphletAnchor::LC, this->hgapsize);
				this->move_to(this->metrics[idx], this->slots[idx], GraphletAnchor::RC, GraphletAnchor::RC, -this->hgapsize);
			}
		}

	public:
		bool can_select(IGraphlet* g) override {
			return (dynamic_cast<Labellet*>(g) != nullptr);
		}

	protected:
		void on_appdata(Windows::Foundation::Uri^ ms_appdata, Metrics^ ftobject) override {
			size_t total = ftobject->db.size();

			this->begin_update_sequence();

			for (size_t idx = 0; idx < this->slot_count; idx++) {
				if (idx < total) {
					this->labels[idx]->set_text(this->master->label_ref(ftobject->db[idx].uuid), GraphletAnchor::LC);
					this->labels[idx]->set_color(color_ref(ftobject->db[idx].label_color));

					this->metrics[idx]->set_text(idx.ToString(), GraphletAnchor::RC);
					this->labels[idx]->set_color(color_ref(ftobject->db[idx].value_color));
				} else {
					this->labels[idx]->set_text(speak("nodatum", metrics_tongue), GraphletAnchor::LC);
					this->labels[idx]->set_color(this->style.nodatum_color);

					this->metrics[idx]->set_text("-", GraphletAnchor::RC);
					this->metrics[idx]->set_color(this->style.nodatum_color);
				}
			}

			this->end_update_sequence();

			this->metrics_config = ftobject;
		}

		Metrics^ on_appdata_not_found(Windows::Foundation::Uri^ ms_appdata) override {
			return ref new Metrics(this->master, this->slot_count);
		}

		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->master->get_logger()->log_message(level, message);
		}

	private: // never deletes these graphlets manually
		Credit<RoundedRectanglet, size_t>** slots;
		Credit<Labellet, size_t>** labels;
		Credit<Labellet, size_t>** metrics;
		Rectanglet* background;

	private:
		Uri^ ms_appdata_config;
		Metrics^ metrics_config;
		MetricsStyle style;

	private:
		size_t slot_count;
		float slot_height;
		float hgapsize;
		float inset;

	private: // never deletes these objects manually
		Metricslet* master;
	};
}

/*************************************************************************************************/
Metricslet::Metricslet(Platform::String^ name, float width, GraphletAnchor anchor, size_t slot_count)
	: Metricslet(make_metrics_style(), name, width, anchor, slot_count) {}

Metricslet::Metricslet(MetricsStyle& style, Platform::String^ name, float width, GraphletAnchor anchor, size_t slot_count)
	: Planetlet(new MetricsFrame(this, name, style, slot_count), width) {
	this->set_stretch_anchor(anchor);
	this->enable_events(true, true);
}

CanvasSolidColorBrush^ Metricslet::label_color_ref(unsigned int idx) {
	return Colours::Foreground;
}

CanvasSolidColorBrush^ Metricslet::value_color_ref(unsigned int idx) {
	return this->label_color_ref(idx);
}
