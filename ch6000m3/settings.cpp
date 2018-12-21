#include <map>

#include "settings.hpp"
#include "planet.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/buttonlet.hpp"

#include "module.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class SettingsCommand { Brightness100, Brightness80, Brightness60, Brightness40, Brightness20, _ };

// WARNING: order matters
private enum class SS : unsigned int {
	Brightness,

	_
};

private class Widget : public Planet {
public:
	Widget(SettingsWidget^ master, PLCMaster* plc) : Planet(__MODULE__), master(master), device(plc) {}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		auto label_font = make_bold_text_format(large_font_size);
		ButtonStyle button_style;
		float button_height;

		this->brightness = this->insert_one(new Labellet(_speak(SS::Brightness), label_font, Colours::GhostWhite));
		this->brightness->fill_extent(0.0F, 0.0F, nullptr, &button_height);

		button_style.font = make_bold_text_format(tiny_font_size);
		button_style.corner_radius = 2.0F;
		button_style.thickness = 1.0F;

		for (SettingsCommand cmd = _E0(SettingsCommand); cmd < SettingsCommand::_; cmd++) {
			auto button = new Credit<Buttonlet, SettingsCommand>(cmd.ToString(), button_height * 1.618F, button_height);

			button->set_style(button_style);
			this->settings[cmd] = this->insert_one(button, cmd);
		}
	}

	void reflow(float width, float height) override {
		IGraphlet* target = this->brightness;
		float xoff = tiny_font_size * 0.5F;

		this->move_to(this->brightness, xoff, height, GraphletAnchor::LB);

		for (SettingsCommand cmd = _E0(SettingsCommand); cmd < SettingsCommand::_; cmd++) {
			this->move_to(this->settings[cmd], target, GraphletAnchor::RC, GraphletAnchor::LC, xoff);
			target = this->settings[cmd];
			xoff = 2.0F;
		}
	}

public:
	void update(long long count, long long interval, long long uptime) override {
		double alpha = this->master->global_mask_alpha;
		Buttonlet* target = nullptr;

		if (alpha > 0.7) {
			target = this->settings[SettingsCommand::Brightness20];
		} else if (alpha > 0.5) {
			target = this->settings[SettingsCommand::Brightness40];
		} else if (alpha > 0.3) {
			target = this->settings[SettingsCommand::Brightness60];
		} else if (alpha > 0.1) {
			target = this->settings[SettingsCommand::Brightness80];
		} else {
			target = this->settings[SettingsCommand::Brightness100];
		}

		for (SettingsCommand cmd = _E0(SettingsCommand); cmd < SettingsCommand::_; cmd++) {
			this->settings[cmd]->set_state(this->settings[cmd] == target, ButtonState::Executing, ButtonState::Ready);
		}
	}

public:
	bool can_select(IGraphlet* g) override {
		auto btn = dynamic_cast<Buttonlet*>(g);

		return ((btn != nullptr) && (btn->get_state() != ButtonState::Disabled));
	}

	void on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
		auto btn = dynamic_cast<Credit<Buttonlet, SettingsCommand>*>(g);

		if (btn != nullptr) {
			double alpha = -1.0;

			switch (btn->id) {
			case SettingsCommand::Brightness100: alpha = 0.0; break;
			case SettingsCommand::Brightness80:  alpha = 0.2; break;
			case SettingsCommand::Brightness60:  alpha = 0.4; break;
			case SettingsCommand::Brightness40:  alpha = 0.6; break;
			case SettingsCommand::Brightness20:  alpha = 0.8; break;
			}

			if (alpha >= 0.0) {
				this->master->global_mask_alpha = alpha;
			}
		}
	}

private:
	SettingsWidget^ master;
	PLCMaster* device;

private: // never delete these graphlets manually.
	std::map<SettingsCommand, Credit<WarGrey::SCADA::Buttonlet, SettingsCommand>*> settings;
	Labellet* brightness;
};

/*************************************************************************************************/
SettingsWidget::SettingsWidget(Syslog* logger, PLCMaster* plc) : UniverseDisplay(logger), plc(plc) {
	this->use_global_mask_setting(false);
}

void SettingsWidget::construct() {
	this->add_planet(new Widget(this, this->plc));
}
