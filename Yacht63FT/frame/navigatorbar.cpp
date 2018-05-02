#include <map>

#include "frame/navigatorbar.hxx"
#include "decorator/background.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/batterylet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
private class NavigatorBoard final {
public:
	NavigatorBoard(Navigatorbar* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei", application_fit_size(28.13F));
		this->menu_font = make_text_format("Microsoft YaHei", application_fit_size(26.51F));
	}

public:
	void load_and_flow(float width, float height) {
		float wc_width = application_fit_size(288.0F);
		float button_height = application_fit_size(84.0F);

		this->working_condition = new BitmapBooleanlet("tapped", wc_width, button_height);

		this->master->insert(this->working_condition, 0.0F, height * 0.5F, GraphletAlignment::LC);
	}

// never deletes these graphlets mannually
private:
	BitmapBooleanlet* working_condition;
		
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ menu_font;
	Navigatorbar* master;
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
	this->action->on_navigate();
}
