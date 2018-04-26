#include "page/statusbar.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

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

private class StatusPage final : public WarGrey::SCADA::MRConfirmation {
public:
	StatusPage(Statusbar* master) : master(master) {
		this->fonts[0] = make_bold_text_format("Microsoft YaHei", application_fit_size(45.0F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(26.25F));
		this->fonts[2] = this->fonts[1];
	}

public:
	void load_and_flow(float width, float height) {
		this->fueltank = new FuelTank(application_fit_size(70.0F) * 0.9F, -1.5714F);
		this->fueltank->set_scale(0.80F);
		this->master->insert(this->fueltank, width * 0.5F, height * 0.5F, GraphletAlignment::CC);
	}

// never deletes these graphlets mannually
private:
	FuelTank* fueltank;
		
private:
	CanvasTextFormat^ fonts[3];
	Statusbar* master;
};

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
