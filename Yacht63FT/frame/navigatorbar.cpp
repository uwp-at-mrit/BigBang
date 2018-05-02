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
		float menu_width = application_fit_size(screen_menu_width);
		float button_width = (width - menu_width) / float(static_cast<unsigned int>(Yacht::_));
		float button_height = application_fit_size(84.0F);
		float button_x = menu_width;
		float button_y = (height - button_height) * 0.5F;
		
		this->menu_background = new BitmapBooleanlet("tapped", menu_width, button_height);
		this->menu_caption = new Labellet("工况", this->caption_font);

		this->master->insert(this->menu_background, 0.0F, button_y);
		this->master->insert(this->menu_caption, this->menu_background, GraphletAlignment::CC, GraphletAlignment::CC);

		for (Yacht id = Yacht::HomePage; id < Yacht::_; id++) {
			this->backgrounds[id] = new Credit<BitmapBooleanlet, Yacht>("tapped", button_width, button_height);
			this->captions[id] = new Credit<Labellet, Yacht>(id.ToString(), this->caption_font);
			this->backgrounds[id]->id = id;
			this->captions[id]->id = id;
			this->master->insert(this->backgrounds[id], button_x, button_y);

			/** WARNING
			 * At this time the bitmaps are not ready, and therefore their location are incorrect,
			 * so labels cannot be located by the alignment relationship.
			 */
			this->master->insert(this->captions[id], button_x + button_width * 0.5F, height * 0.5F, GraphletAlignment::CC);

			button_x += button_width;
		}
	}

// never deletes these graphlets mannually
private:
	BitmapBooleanlet* menu_background;
	Labellet* menu_caption;
	std::map<Yacht, Credit<BitmapBooleanlet, Yacht>*> backgrounds;
	std::map<Yacht, Credit<Labellet, Yacht>*> captions;
		
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
