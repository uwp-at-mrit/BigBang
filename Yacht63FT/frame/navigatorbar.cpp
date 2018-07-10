#include <map>

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

private class Backgroundlet : public OptionBitmaplet {
public:
	Backgroundlet(float width, float height) : OptionBitmaplet("active", "inactive", width, height) {}

public:
	void draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		// this override does nothing but disabling the default behaviours
	}
};

private class CreditItemlet : public virtual UnionBitmaplet<YachtStatus> {
public:
	CreditItemlet(Yacht id, float width, float height, CanvasTextFormat^ font) : UnionBitmaplet<YachtStatus>(id.ToString(), width, height), id(id) {
		this->caption = make_text_layout(speak(id), font);
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
		UnionBitmaplet<YachtStatus>::on_appx(ms_appx, doc_bmp, hint);

		if (this->icon_window.Width == 0.0F) {
			float icon_width = design_to_application_width(doc_bmp->Size.Width);
			float icon_height = design_to_application_height(doc_bmp->Size.Height);
			float icon_size = std::fmin(icon_width, icon_height);

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
private class NavigatorBoard final : public WarGrey::SCADA::IPLCStatusListener {
public:
	NavigatorBoard(Navigatorbar* master) : master(master) {
		this->font = make_text_format("Microsoft YaHei", design_to_application_height(28.13F));
	}

public:
	void load_and_flow(float width, float height) {
		float button_gapsize = design_to_application_width(1.0F);
		float button_cell_width = width / float(_N(Yacht));
		float button_width = button_cell_width - button_gapsize;
		float button_height = design_to_application_height(84.0F);
		float button_x = button_gapsize * 0.5F;
		float button_y = (height - button_height) * 0.5F;

		for (Yacht id = Yacht::HomePage; id < Yacht::_; id++) {
			this->backgrounds[id] = new Backgroundlet(button_width, button_height);
			this->items[id] = new CreditItemlet(id, button_width, button_height, this->font);
			this->master->insert(this->backgrounds[id], button_x, button_y);
			this->master->insert(this->items[id], button_x, button_y);

			if (id == Yacht::HomePage) {
				this->backgrounds[id]->set_value(true);
				this->selected_id = id;
			}

			button_x += button_cell_width;
		}
	}

	void on_click(Yacht id) {
		if (this->selected_id != id) {
			this->backgrounds[this->selected_id]->set_value(false);
			this->backgrounds[id]->set_value(true);
			this->selected_id = id;
		}
	}

public:
	void on_plc_connectivity_changed(IPLCMaster* device, bool connected) override {
		YachtStatus s = (connected ? YachtStatus::Running : YachtStatus::Stopped);
		
		this->master->begin_update_sequence();

		for (auto it = items.begin(); it != items.end(); it++) {
			it->second->set_value(s, true);
		}

		this->master->end_update_sequence();
	}

// never deletes these graphlets mannually
private:
	std::map<Yacht, Backgroundlet*> backgrounds;
	std::map<Yacht, CreditItemlet*> items;
		
private:
	CanvasTextFormat^ font;
	Navigatorbar* master;
	Yacht selected_id;
};

/*************************************************************************************************/
std::map<Navigatorbar*, NavigatorBoard*> dashboards;

Navigatorbar::Navigatorbar(IMRMaster* device, INavigatorAction^ action)
	: Planet(":navigatorbar:"), device(device), action(action) {}

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

		this->device->append_plc_status_listener(dashboard);
	}
}

void Navigatorbar::on_navigated_to(Yacht page) {
	auto maybe_dashboard = dashboards.find(this);

	if (maybe_dashboard != dashboards.end()) {
		maybe_dashboard->second->on_click(page);
	}
}

void Navigatorbar::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	auto credit_item = dynamic_cast<CreditItemlet*>(g);

	if (credit_item != nullptr) {
		this->on_navigated_to(credit_item->id);
		this->action->on_navigate(credit_item->id);
	}
}
