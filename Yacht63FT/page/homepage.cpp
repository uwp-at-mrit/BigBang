#include <map>

#include "page/homepage.hpp"
#include "decorator/background.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "system.hpp"

#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class HomeBoard final {
public:
	HomeBoard(Homepage* master) : master(master) {
		this->fonts[0] = make_bold_text_format("Microsoft YaHei", application_fit_size(45.0F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(26.25F));
		this->fonts[2] = this->fonts[1];
	}

public:
	void load_and_flow(float width, float height) {
		Platform::String^ copyright_items[] = { ":system:", ":author:", ":version:" };
		
		this->yacht = new Bitmaplet("yacht", 0.0F, height * 0.9F);
		this->master->insert(this->yacht, width * 0.5F, height * 0.5F, GraphletAlignment::CC);
		
		for (size_t i = 0; i < sizeof(this->copyright) / sizeof(Labellet*); i++) {
			this->copyright[i] = new Labellet(speak(copyright_items[i]));
			this->copyright[i]->set_font(this->fonts[i]);
			this->copyright[i]->set_color(Colours::GhostWhite);

			if (i == 0) {
				this->master->insert(this->copyright[i],
					application_fit_size(screen_copyright_xoff),
					application_fit_size(screen_copyright_yoff));
			} else {
				this->master->insert(this->copyright[i],
					this->copyright[i - 1], GraphletAlignment::LB,
					GraphletAlignment::LT);
			}
		}
	}

// never deletes these graphlets mannually
private:
	Bitmaplet* yacht;
	Labellet* copyright[3];
		
private:
	CanvasTextFormat^ fonts[3];
	Homepage* master;
};

/*************************************************************************************************/
std::map<Homepage*, HomeBoard*> dashboards;

Homepage::Homepage() : Planet(":homepage:") {
	this->set_decorator(new BackgroundDecorator(0x1E1E1E, 0.0F, 0.0F, 1.0F, 0.0F));
}

Homepage::~Homepage() {
	auto maybe_dashboard = dashboards.find(this);

	if (maybe_dashboard != dashboards.end()) {
		delete maybe_dashboard->second;
		dashboards.erase(maybe_dashboard);
	}
}

void Homepage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (dashboards.find(this) == dashboards.end()) {
		HomeBoard* dashboard = new HomeBoard(this);

		dashboards.insert(std::pair<Homepage*, HomeBoard*>(this, dashboard));
		dashboard->load_and_flow(width, height);
	}
}

void Homepage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
