#include "page/statusbar.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/batterylet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class StatusPage final : public WarGrey::SCADA::MRConfirmation {
public:
	StatusPage(Statusbar* master) : master(master) {
		this->fonts[0] = make_bold_text_format("Microsoft YaHei", application_fit_size(45.0F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(26.25F));
		this->fonts[2] = this->fonts[1];
	}

public:
	void load_and_flow(float width, float height) {
		float icon_width = application_fit_size(70.0F) * 0.9F;
		float center_x = width * 0.5F;
		float center_y = height * 0.5F;

		// NOTE: the background content may be a rounded rectangle
		this->background = new Bitmaplet("workspace_bg", width * 1.2F, height - 5.0F);
		this->fueltank = new FuelTanklet(icon_width, -1.5714F);
		this->battery = new Batterylet(icon_width);

		this->fueltank->set_scale(0.80F);
		this->battery->set_scale(0.16F);

		this->master->insert(this->background, center_x, 0.0F, GraphletAlignment::CT);
		this->master->insert(this->fueltank, center_x - icon_width, center_y, GraphletAlignment::RC);
		this->master->insert(this->battery, center_x + icon_width, center_y, GraphletAlignment::LC);
	}

// never deletes these graphlets mannually
private:
	Bitmaplet* background;
	FuelTanklet* fueltank;
	Batterylet* battery;
		
private:
	CanvasTextFormat^ fonts[3];
	Statusbar* master;
};

/*************************************************************************************************/
Statusbar::Statusbar() : Planet(":statusbar:") {
	this->console = new StatusPage(this);
}

Statusbar::~Statusbar() {
	if (this->console != nullptr) {
		delete this->console;
	}
}

void Statusbar::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<StatusPage*>(this->console);
	
	if (console != nullptr) {
		console->load_and_flow(width, height);
	}
}

void Statusbar::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
