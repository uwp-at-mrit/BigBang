#include <map>

#include "widget/settings.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/buttonlet.hpp"

#include "system.hpp"
#include "module.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasSolidColorBrush^ region_background = Colours::make(diagnostics_region_background);
static CanvasSolidColorBrush^ settings_background = Colours::make(diagnostics_alarm_background);
static CanvasSolidColorBrush^ settings_foreground = Colours::Silver;
static CanvasSolidColorBrush^ caption_foreground = Colours::make(diagnostics_caption_foreground);
static CanvasSolidColorBrush^ caption_background = Colours::make(diagnostics_caption_background);

// WARNING: order matters
private enum class Mode { psTrunnion, psIntermediate, psDragHead, sbTrunnion, sbIntermediate, sbDragHead, _ };

private enum class GWSS {
	_Gantry, GantryFlow, PushFlow, PullFlow, GantryPressure,
	
	_Winch, WinchFlow, PushUpFlow, PushOutFlow, PullUpFlow, PullOutFlow, WinchPressure,

	_
};

/*************************************************************************************************/
private class Settings : public ISatellite, public PLCConfirmation {
public:
	Settings(PLCMaster* device) : ISatellite(default_logging_level, __MODULE__), gapsize(normal_font_size), device(device) {
		this->caption_font = make_bold_text_format("Consolas", large_font_size);
		this->label_font = make_bold_text_format("Consolas", normal_font_size);
		this->setting_font = make_bold_text_format("Microsoft YeHei", normal_font_size);
		this->settings_style = make_highlight_dimension_style(large_font_size, 6U, 1);

		this->button_style.font = make_bold_text_format("Microsoft YeHei", normal_font_size);
		this->button_style.corner_radius = 3.0F;
		this->button_style.thickness = 2.0F;
	}

	void Settings::fill_satellite_extent(float* width, float* height) {
		SET_VALUES(width, 800.0F, height, 800.0F);
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		float caption_width = width * 0.618F;
		float caption_height = this->caption_font->FontSize * 1.618F;
		float settings_height = this->setting_font->FontSize * 2.4F;
		float settings_corner_radius = 8.0F;
		float button_width = width - caption_width - this->gapsize * 4.0F;
		float button_height = this->button_style.font->FontSize * 2.4F;

		this->change_mode(0U);
		this->load_buttons(this->tabs, button_width, button_height);
		
		for (Mode pos = _E(Mode, 0); pos < Mode::_; pos++) {
			unsigned int idx = _I(pos);

			this->change_mode(idx + 2U);

			this->caption_boxes[pos] = this->insert_one(new Rectanglet(caption_width, caption_height, caption_background));
			this->captions[pos] = this->insert_one(new Labellet(_speak(pos), this->caption_font, caption_foreground));

			for (GWSS id = _E(GWSS, 0); id < GWSS::_; id++) {
				const wchar_t* lbl_cpt = id.ToString()->Data();

				if (lbl_cpt[0] == L'_') {
					Credit<Labellet, GWSS>* label = nullptr;

					switch (pos) {
					case Mode::psDragHead: case Mode::psIntermediate: case Mode::psTrunnion: {
						label = new Credit<Labellet, GWSS>(_speak(id), this->label_font, default_ps_color);
					}; break;
					case Mode::sbDragHead: case Mode::sbIntermediate: case Mode::sbTrunnion: {
						label = new Credit<Labellet, GWSS>(_speak(id), this->label_font, default_sb_color);
					}; break;
					}

					this->dps_labels[idx][id] = this->insert_one(label, id);
				} else {
					auto label = new Credit<Labellet, GWSS>(_speak(id), this->setting_font);
					auto setting = new Credit<Dimensionlet, GWSS>(DimensionState::Input, this->settings_style, "bar");
					auto box = new Credit<RoundedRectanglet, GWSS>(caption_width, settings_height, settings_corner_radius, settings_background);

					this->dps_setting_boxes[idx][id] = this->insert_one(box, id);
					this->dps_labels[idx][id] = this->insert_one(label, id);
					this->dps_settings[idx][id] = this->insert_one(setting, id);
				}
			}
		}
	}

	void reflow(float width, float height) override {
		float button_x = this->gapsize;
		float button_y = this->gapsize;
		float caption_rx = width - this->gapsize;
		float caption_y = this->gapsize;
		float button_width, button_height;
		
		this->tabs[Mode::psDragHead]->fill_extent(0.0F, 0.0F, &button_width, &button_height);

		for (Mode pos = _E(Mode, 0); pos < Mode::_; pos++) {
			float caption_width, caption_height, setting_height;
			float settings_y = caption_y;
			float inset = this->gapsize * 0.42F;
			unsigned int idx = _I(pos);

			this->change_mode(0U);

			this->move_to(this->tabs[pos], button_x, button_height * _F(pos) + button_y);

			this->change_mode(idx + 2U);

			this->caption_boxes[pos]->fill_extent(0.0F, 0.0F, &caption_width, &caption_height);
			this->move_to(this->caption_boxes[pos], caption_rx, caption_y, GraphletAnchor::RT);
			this->move_to(this->captions[pos], caption_rx - caption_width * 0.5F, caption_y + caption_height * 0.5F, GraphletAnchor::CC);

			settings_y += (caption_height + inset);
			for (GWSS id = _E(GWSS, 0); id < GWSS::_; id++) {
				if (this->dps_settings[idx].find(id) == this->dps_settings[idx].end()) {
					this->dps_labels[idx][id]->fill_extent(0.0F, 0.0F, nullptr, &setting_height);
					this->move_to(this->dps_labels[idx][id], caption_rx - caption_width * 0.5F, settings_y, GraphletAnchor::CT);
				} else {
					this->dps_setting_boxes[idx][id]->fill_extent(0.0F, 0.0F, nullptr, &setting_height);
					this->move_to(this->dps_setting_boxes[idx][id], caption_rx, settings_y, GraphletAnchor::RT);
					this->move_to(this->dps_labels[idx][id], this->dps_setting_boxes[idx][id], GraphletAnchor::LC, GraphletAnchor::LC, inset);
					this->move_to(this->dps_settings[idx][id], this->dps_setting_boxes[idx][id], GraphletAnchor::RC, GraphletAnchor::RC, -inset);
				}

				settings_y += (setting_height + inset);
			}
		}

		this->change_mode(1U);
	}

public:
	void update(long long count, long long interval, long long uptime) override {
		Mode mode = _E(Mode, this->current_mode() - 2U);

		for (auto it = this->tabs.begin(); it != this->tabs.end(); it++) {
			it->second->set_state(it->second->id == mode, ButtonState::Executing, ButtonState::Ready);
		}
	}

public:
	bool Settings::can_select(IGraphlet* g) override {
		auto btn = dynamic_cast<Buttonlet*>(g);

		return ((btn != nullptr) && (btn->get_state() != ButtonState::Disabled));
	}

	void Settings::on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
		auto tab = dynamic_cast<Credit<Buttonlet, Mode>*>(g);

		if (tab != nullptr) {
			this->change_mode(_I(tab->id) + 2U);
		}
	}

	void Settings::on_focus(IGraphlet* g) override {
		auto editor = dynamic_cast<IEditorlet*>(g);

		if (editor != nullptr) {
			this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		}
	}


	bool on_key(VirtualKey key, bool wargrey_keyboard) override {
		bool handled = Planet::on_key(key, wargrey_keyboard);

		if ((!handled) && (this->device->get_mode() != PLCMasterMode::User)) {
			auto editor = dynamic_cast<Credit<Dimensionlet, GWSS>*>(this->get_focus_graphlet());
			
			if (editor != nullptr) {
				float v = float(editor->get_input_number());

				if (v > 0.0F) {
					this->get_logger()->log_message(Log::Info, v.ToString());
				}

				this->hide_virtual_keyboard();
				this->set_caret_owner(nullptr);

				handled = true;
			}
		}

		return handled;
	}

private:
	template<typename CMD>
	void load_buttons(std::map<CMD, Credit<Buttonlet, CMD>*>& bs, float width, float height) {
		for (CMD cmd = _E0(CMD); cmd < CMD::_; cmd++) {
			bs[cmd] = this->insert_one(new Credit<Buttonlet, CMD>(cmd.ToString(), width, height, __MODULE__), cmd);
			bs[cmd]->set_style(this->button_style);
		}
	}

private: // never delete these graphlets manually.
	std::map<Mode, Labellet*> captions;
	std::map<Mode, Rectanglet*> caption_boxes;
	std::map<GWSS, Credit<Labellet, GWSS>*> dps_labels[_N(Mode)];
	std::map<GWSS, Credit<Dimensionlet, GWSS>*> dps_settings[_N(Mode)];
	std::map<GWSS, Credit<RoundedRectanglet, GWSS>*> dps_setting_boxes[_N(Mode)];

private:
	std::map<Mode, Credit<Buttonlet, Mode>*> tabs;

private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ setting_font;
	DimensionStyle settings_style;
	ButtonStyle button_style;

private:
	float gapsize;

private:
	PLCMaster* device;
};

/*************************************************************************************************/
ISatellite* WarGrey::SCADA::make_settings(PLCMaster* plc) {
	return new Settings(plc);
}
