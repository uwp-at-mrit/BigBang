#include "page/homepage.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"

#include "tongue.hpp"
#include "system.hpp"

#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class DefaultPage final : public WarGrey::SCADA::MRConfirmation {
public:
	DefaultPage(Homepage* master) : master(master) {
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
				this->master->insert(this->copyright[i], application_fit_size(screen_copyright_xoff), application_fit_size(screen_copyright_yoff));
			} else {
				this->master->insert(this->copyright[i], this->copyright[i - 1], GraphletAlignment::LB, GraphletAlignment::LT);
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

Homepage::Homepage() : Planet(":homepage:") {
	this->console = new DefaultPage(this);
}

Homepage::~Homepage() {
	if (this->console != nullptr) {
		delete this->console;
	}
}

void Homepage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<DefaultPage*>(this->console);
	
	if (console != nullptr) {
		console->load_and_flow(width, height);
	}
}

void Homepage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
