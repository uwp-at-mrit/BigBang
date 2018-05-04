﻿#include <map>

#include "frame/navigatorbar.hxx"
#include "decorator/background.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/batterylet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "credit.hpp"
#include "tongue.hpp"
#include "text.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class YachtStatus { Stopped, Running, Alerting, _ };

private class Backgroundlet : public BitmapBooleanlet {
public:
	Backgroundlet(float width, float height) : BitmapBooleanlet("active", "inactive", width, height) {}

public:
	void draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		// this override does nothing but disabling the default behaviours
	}
};

private class CreditItemlet : public virtual BitmapStatelet<YachtStatus> {
public:
	CreditItemlet(Yacht id, float width, float height, CanvasTextFormat^ font) : BitmapStatelet<YachtStatus>(id.ToString(), width, height), id(id) {
		this->caption = make_text_layout(speak(id.ToString()), font);
		this->cpt_xoff = (this->window.Width - this->caption->LayoutBounds.Width) * 0.5F;
		this->cpt_yoff = (this->window.Height - this->caption->LayoutBounds.Height) * 0.5F;
	}

	Yacht id;

public:
	void draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		this->icon_window.X = x + icon_xoff;
		this->icon_window.Y = y + icon_yoff;
		
		ds->DrawImage(this->graph_bmps[this->get_value()], this->icon_window);
	}

	void draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		ICanvasBrush^ caption_color = Colours::GhostWhite;

		switch (this->get_value()) {
		case YachtStatus::Stopped:  caption_color = Colours::LightGray; break;
		case YachtStatus::Alerting: caption_color = Colours::Firebrick; break;
		}

		ds->DrawTextLayout(this->caption, x + this->cpt_xoff, y + this->cpt_yoff, caption_color);
	}

protected:
	void on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp, YachtStatus hint) override {
		BitmapStatelet<YachtStatus>::on_appx(ms_appx, doc_bmp, hint);

		if (this->icon_window.Width == 0.0F) {
			float icon_size = application_fit_size(std::fmin(doc_bmp->Size.Width, doc_bmp->Size.Height));

			this->icon_window.Width = icon_size;
			this->icon_window.Height = icon_size;
			this->icon_xoff = (this->window.Width - icon_size) * 0.5F;
			this->icon_yoff = (this->window.Height - icon_size) * 0.5F;
		}
	}

private:
	Rect icon_window;
	float icon_xoff;
	float icon_yoff;

private:
	CanvasTextLayout^ caption;
	float cpt_xoff;
	float cpt_yoff;
};

/*************************************************************************************************/
private class NavigatorBoard final {
public:
	NavigatorBoard(Navigatorbar* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei", application_fit_size(28.13F));
		this->menu_font = make_text_format("Microsoft YaHei", application_fit_size(26.51F));
	}

public:
	void load_and_flow(float width, float height) {
		float menu_width = application_fit_size(screen_menu_width);
		float button_width = (width - menu_width) / float(static_cast<unsigned int>(Yacht::_));
		float button_height = application_fit_size(84.0F);
		float button_x = menu_width;
		float button_y = (height - button_height) * 0.5F;
		
		this->menu_background = new Backgroundlet(menu_width, button_height);
		this->menu_caption = new Labellet("工况", this->caption_font);

		this->menu_background->set_value(true);

		this->master->insert(this->menu_background, 0.0F, button_y);
		this->master->insert(this->menu_caption, this->menu_background, GraphletAlignment::CC, GraphletAlignment::CC);

		for (Yacht id = Yacht::HomePage; id < Yacht::_; id++) {
			this->backgrounds[id] = new Backgroundlet(button_width, button_height);
			this->items[id] = new CreditItemlet(id, button_width, button_height, this->caption_font);
			this->master->insert(this->backgrounds[id], button_x, button_y);
			this->master->insert(this->items[id], button_x, button_y);

			if (id == Yacht::HomePage) {
				this->backgrounds[id]->set_value(true);
				this->selected_id = id;
			}

			button_x += button_width;
		}
	}

	void on_click(Yacht id) {
		if (this->selected_id != id) {
			this->backgrounds[this->selected_id]->set_value(false);
			this->backgrounds[id]->set_value(true);
			this->selected_id = id;
		}
	}

// never deletes these graphlets mannually
private:
	Backgroundlet* menu_background;
	Labellet* menu_caption;
	std::map<Yacht, Backgroundlet*> backgrounds;
	std::map<Yacht, CreditItemlet*> items;
		
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ menu_font;
	Navigatorbar* master;
	Yacht selected_id;
};

/*************************************************************************************************/
std::map<Navigatorbar*, NavigatorBoard*> dashboards;

Navigatorbar::Navigatorbar(INavigatorAction^ action) : Planet(":navigatorbar:"), action(action) {}

Navigatorbar::~Navigatorbar() {
	auto maybe_dashboard = dashboards.find(this);

	if (maybe_dashboard != dashboards.end()) {
		delete maybe_dashboard->second;
		dashboards.erase(maybe_dashboard);
	}
}

void Navigatorbar::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (dashboards.find(this) == dashboards.end()) {
		NavigatorBoard* dashboard = new NavigatorBoard(this);

		dashboards.insert(std::pair<Navigatorbar*, NavigatorBoard*>(this, dashboard));
		dashboard->load_and_flow(width, height);

		this->set_decorator(new BackgroundDecorator(0x1E1E1E));
	}
}

void Navigatorbar::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	auto credit_item = dynamic_cast<CreditItemlet*>(g);

	if (credit_item != nullptr) {
		auto maybe_dashboard = dashboards.find(this);

		if (maybe_dashboard != dashboards.end()) {
			maybe_dashboard->second->on_click(credit_item->id);
		}

		this->action->on_navigate(credit_item->id);
	}
}