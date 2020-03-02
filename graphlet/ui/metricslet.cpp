#include <map>
#include <vector>

#include "graphlet/ui/metricslet.hpp"

#include "graphlet/ui/textlet.hpp"
#include "graphlet/shapelet.hpp"

#include "filesystem/msappdata.hxx"
#include "satellite/colorpicker.hpp"

#include "datum/credit.hpp"
#include "datum/string.hpp"
#include "datum/fixnum.hpp"
#include "datum/file.hpp"

#include "module.hpp"
#include "brushes.hxx"
#include "planet.hpp"
#include "menu.hpp"

#include "colorspace.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
static Platform::String^ metrics_file_type = "metrics style";
static Platform::String^ metrics_tongue = "metrics";

static CanvasTextFormat^ default_metrics_label_font = make_text_format("Microsoft YaHei", 18.0F);
static CanvasTextFormat^ default_metrics_font = make_bold_text_format("Cambria Math", 20.0F);

static unsigned int default_slot_count = 20U;
static std::map<unsigned int, CanvasSolidColorBrush^> colors;

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
	private enum class MetricsAction { SwitchDimension, ColorizeLabel, ColorizeMetric, ConcealDimension, _ };

	private struct Metric {
		unsigned int dimension_idx;
		CanvasSolidColorBrush^ label_color;
		CanvasSolidColorBrush^ value_color;
		bool visible;
	};

	private ref class Metrics sealed {
	public:
		static Metrics^ load(Platform::String^ path) {
			Metrics^ self = nullptr;
			std::filebuf src;

			if (open_input_binary(src, path)) {
				self = ref new Metrics();

				while (peek_char(src) != EOF) {
					Metric m;

					m.dimension_idx = (unsigned int)read_natural(src);
					m.label_color = color_ref((unsigned int)read_natural(src));
					m.value_color = color_ref((unsigned int)read_natural(src));
					m.visible = read_bool(src);

					self->db.push_back(m);

					discard_this_line(src);
				}
			}

			return self;
		}
		
		static bool save(Metrics^ self, Platform::String^ path) {
			std::wofstream m_config;
			bool okay = false;

			if (open_output_binary(m_config, path)) {
				for (auto m = self->db.begin(); m != self->db.end(); m++) {
					m_config << " " << m->dimension_idx;
					m_config << " " << color_to_hexadecimal(m->label_color->Color);
					m_config << " " << color_to_hexadecimal(m->value_color->Color);
					write_bool(m_config << " ", m->visible);
					write_newline(m_config);
				}
			}

			return okay;
		}

	public:
		Metrics(Metrics^ src = nullptr) {
			this->refresh(src);
		}

	public:
		void refresh(Metrics^ src) {
			if ((this != src) && (src != nullptr)) {
				this->db.clear();

				for (auto it = src->db.begin(); it != src->db.end(); it++) {
					this->db.push_back(*it);
				}
			}
		}

	internal:
		Metrics(IMetricsProvider* master, unsigned int slot_count) {
			unsigned int total = fxmin(slot_count, master->capacity());

			for (unsigned int idx = 0; idx < total; idx++) {
				Metric m;

				m.dimension_idx = idx;
				m.label_color = master->label_color_ref(idx);
				m.value_color = master->value_color_ref(idx);
				m.visible = true;

				this->db.push_back(m);
			}
		}

	internal:
		std::vector<Metric> db;
	};

	private class Metriclet : public Labellet {
	public:
		Metriclet(IMetricsProvider* master, unsigned int idx, CanvasTextFormat^ font, ICanvasBrush^ color)
			: Labellet("-", font, color), master(master), dimension_idx(idx), value(0.0) { }

	public:
		void update(long long count, long long interval, long long uptime) override {
			if (this->dimension_idx < this->master->capacity()) {
				double current_value = this->master->value_ref(this->dimension_idx);

				if (this->value != current_value) {
					this->value = current_value;
					this->set_text(flstring(this->value, 2), GraphletAnchor::RC);
				}
			} else if (!flisnan(this->value)) {
				this->value = flnan;
				this->set_text("-", GraphletAnchor::RC);
			}
		}

	public:
		unsigned int dimension_idx;

	private:
		double value;

	private:
		IMetricsProvider* master;
	};

	private class MetricsFrame final
		: public virtual Planet
		, public virtual IMsAppdata<Metrics>
		, public virtual IColorPickerReceiver
		, public virtual IMenuCommand<MetricsAction, Credit<RoundedRectanglet, unsigned int>, MetricsFrame*>
		, public virtual IMenuCommand<unsigned int, void, MetricsFrame*> {
	public:
		MetricsFrame(IMetricsProvider* master, Platform::String^ name, MetricsStyle& style, unsigned int slots)
			: Planet(name), master(master), style(style), slot_count(slots > 0 ? slots : default_slot_count), metrics_config(nullptr) {
			this->ms_appdata_config = ms_appdata_file(name, ".config", "metrics");

			prepare_metrics_style(&this->style);
			this->slot_height = this->style.metrics_font->FontSize * 1.2F;
			this->inset = this->style.metrics_font->FontSize * 0.618F;
			this->hgapsize = this->inset * 0.618F;

			this->slots = new Credit<RoundedRectanglet, unsigned int>*[this->slot_count];
			this->labels = new Credit<Labellet, unsigned int>*[this->slot_count];
			this->metrics = new Credit<Metriclet, unsigned int>*[this->slot_count];

			this->metrics_cmd_menu = make_menu<MetricsAction, Credit<RoundedRectanglet, unsigned int>, MetricsFrame*>(this, this, metrics_tongue);
			this->metrics_menu = ref new MenuFlyout();

			if (this->master != nullptr) {
				this->master->reference();

				for (unsigned int idx = 0; idx < this->master->capacity(); idx++) {
					menu_push_command(this->metrics_menu,
						make_wstring(L"%d: %s", idx + 1, this->master->label_ref(idx)->Data()),
						ref new MenuCommand<unsigned int, void, MetricsFrame*>(this, idx, this));
				}
			}

			menu_push_command(this->metrics_menu, speak("unset", metrics_tongue),
				ref new MenuCommand<unsigned int, void, MetricsFrame*>(this, this->master->capacity(), this));
		}

		~MetricsFrame() noexcept {
			delete[] this->slots;
			delete[] this->labels;
			delete[] this->metrics;

			if (this->master != nullptr) {
				this->master->destroy();
			}
		}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override {
			float corner_radius = 8.0F;
			float slot_width = width - this->inset * 2.0F;
			float bgheight = (this->slot_height + this->hgapsize) * float(this->slot_count) + this->inset * 2.0F;

			this->background = this->insert_one(new Rectanglet(width - this->style.border_thickness, bgheight,
				this->style.background_color, this->style.border_color, this->style.border_thickness));

			for (unsigned int idx = 0; idx < this->slot_count; idx++) {
				ICanvasBrush^ color = Colours::Foreground;

				this->slots[idx] = this->insert_one(new Credit<RoundedRectanglet, unsigned int>(
					slot_width, this->slot_height, corner_radius, this->style.slot_color_color, this->style.slot_border_color), idx);

				this->labels[idx] = this->insert_one(new Credit<Labellet, unsigned int>(speak("loading", metrics_tongue), this->style.label_font, color), idx);
				this->metrics[idx] = this->insert_one(new Credit<Metriclet, unsigned int>(this->master, idx, this->style.metrics_font, color), idx);

				// so that tapping label and metrics as well as their slot will popup the menu
				this->labels[idx]->camouflage(true);
				this->metrics[idx]->camouflage(true);
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
			return (dynamic_cast<RoundedRectanglet*>(g) != nullptr);
		}

		void on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
			menu_popup(this->metrics_cmd_menu, g, local_x, local_y);
		}

		void execute(MetricsAction cmd, Credit<RoundedRectanglet, unsigned int>* slot, MetricsFrame* self) override {
			switch (cmd) {
			case MetricsAction::SwitchDimension: menu_popup(this->metrics_menu, slot, 0.0F, 0.0F); break;
			case MetricsAction::ColorizeLabel: WarGrey::DTPM::ColorPicker::get_instance(Palette::X11)->show(this, this->labels[slot->id]); break;
			case MetricsAction::ColorizeMetric: WarGrey::DTPM::ColorPicker::get_instance(Palette::X11)->show(this, this->metrics[slot->id]); break;
			case MetricsAction::ConcealDimension: {
				this->metrics_config->db[slot->id].visible = !this->metrics_config->db[slot->id].visible;
				this->store(this->ms_appdata_config, this->metrics_config, metrics_file_type);
			}; break;
			}
		}

		void execute(unsigned int dim_idx, void* nil, MetricsFrame* self) override {
			auto slot = dynamic_cast<Credit<RoundedRectanglet, unsigned int>*>(this->find_next_selected_graphlet());

			if (slot != nullptr) {
				this->metrics[slot->id]->dimension_idx = dim_idx;
				this->metrics_config->db[slot->id].dimension_idx = dim_idx;
				this->metrics_config->db[slot->id].visible = true;
				this->store(this->ms_appdata_config, this->metrics_config, metrics_file_type);
			}
		}

		void on_color_pick(CanvasSolidColorBrush^ color, IGraphlet* target) override {
			auto l = dynamic_cast<Credit<Labellet, unsigned int>*>(target);
			auto m = dynamic_cast<Credit<Metriclet, unsigned int>*>(target);

			if (l != nullptr) {
				if (l->id < this->metrics_config->db.size()) {
					this->metrics_config->db[l->id].label_color = color;
					this->store(this->ms_appdata_config, this->metrics_config, metrics_file_type);
				}
			} else if (m != nullptr) {
				if (m->id < this->metrics_config->db.size()) {
					this->metrics_config->db[m->id].value_color = color;
					this->store(this->ms_appdata_config, this->metrics_config, metrics_file_type);
				}
			}
		}

		bool delete_me() override {
			return false;
		}

	protected:
		void on_appdata(Windows::Foundation::Uri^ ms_appdata, Metrics^ ftobject) override {
			std::map<unsigned int, bool> dimensions;
			size_t total = ftobject->db.size();
			
			this->begin_update_sequence();

			for (unsigned int idx = 0; idx < this->slot_count; idx++) {
				float opacity = (ftobject->db[idx].visible ? 100.0F : 0.0F);
				
				this->cellophane(this->labels[idx], opacity);
				this->cellophane(this->metrics[idx], opacity);

				this->metrics[idx]->dimension_idx = ftobject->db[idx].dimension_idx;

				if ((idx < total) && (ftobject->db[idx].dimension_idx < this->master->capacity())) {
					this->labels[idx]->set_text(this->master->label_ref(this->metrics[idx]->dimension_idx), GraphletAnchor::LC);
					this->labels[idx]->set_color(ftobject->db[idx].label_color);
					this->metrics[idx]->set_color(ftobject->db[idx].value_color);
					dimensions.insert(std::pair<unsigned int, bool>(this->metrics[idx]->dimension_idx, ftobject->db[idx].visible));
				} else {
					this->labels[idx]->set_text(speak("nodatum", metrics_tongue), GraphletAnchor::LC);
					this->labels[idx]->set_color(this->style.nodatum_color);
					this->metrics[idx]->set_color(this->style.nodatum_color);
				}
			}

			this->end_update_sequence();

			ui_thread_run_async([=]() {
				for (unsigned int idx = 0; idx < this->master->capacity(); idx++) {
					auto dim = dimensions.find(idx);
					if (dim == dimensions.end()) {
						menu_set_foreground_color(this->metrics_menu, idx, Colours::Foreground);
					} else if (dim->second) {
						menu_set_foreground_color(this->metrics_menu, idx, Colours::Green);
					} else {
						menu_set_foreground_color(this->metrics_menu, idx, Colours::ForestGreen);
					}
				}});

			this->metrics_config = ftobject;
		}

		Metrics^ on_appdata_not_found(Windows::Foundation::Uri^ ms_appdata) override {
			return ref new Metrics(this->master, this->slot_count);
		}

		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->get_logger()->log_message(level, message);
		}

	private: // never deletes these graphlets manually
		Credit<RoundedRectanglet, unsigned int>** slots;
		Credit<Labellet, unsigned int>** labels;
		Credit<Metriclet, unsigned int>** metrics;
		Rectanglet* background;

	private:
		Uri^ ms_appdata_config;
		Metrics^ metrics_config;
		MetricsStyle style;

	private:
		MenuFlyout^ metrics_menu;
		MenuFlyout^ metrics_cmd_menu;

	private:
		unsigned int slot_count;
		float slot_height;
		float hgapsize;
		float inset;

	private: // never deletes these objects manually
		IMetricsProvider* master;
	};
}

/*************************************************************************************************/
Metricslet::Metricslet(IMetricsProvider* master, Platform::String^ name, float width, GraphletAnchor anchor, unsigned int slot_count)
	: Metricslet(make_metrics_style(), master, name, width, anchor, slot_count) {}

Metricslet::Metricslet(MetricsStyle& style, IMetricsProvider* master, Platform::String^ name, float width, GraphletAnchor anchor, unsigned int slot_count)
	: Planetlet(new MetricsFrame(master, name, style, slot_count), width) {
	this->set_stretch_anchor(anchor);
	this->enable_events(true, true);
}

/*************************************************************************************************/
CanvasSolidColorBrush^ IMetricsProvider::label_color_ref(unsigned int idx) {
	return Colours::Foreground;
}

CanvasSolidColorBrush^ IMetricsProvider::value_color_ref(unsigned int idx) {
	return this->label_color_ref(idx);
}
