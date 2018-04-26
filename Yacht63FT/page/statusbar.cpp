#include "page/statusbar.hpp"
#include "decorator/background.hpp"
#include "decorator/cell.hpp"
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

private class StatusPage final : public WarGrey::SCADA::MRConfirmation {
public:
	~StatusPage() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	StatusPage(Statusbar* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->fonts[0] = make_text_format("Microsoft YaHei", application_fit_size(30.0F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(41.27F));
		
		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		IGraphlet* target = nullptr;
		float cell_x, cell_y, cell_width, cell_height;
		float icon_width = application_fit_size(70.0F) * 0.9F;
		float label_offset = application_fit_size(138.0F);

		for (unsigned int i = 0; i < 3; i ++) {
			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);
			
			switch (i) {
			case 0: this->fueltank = new FuelTanklet(icon_width, -1.5714F); target = this->fueltank; break;
			case 1: this->battery = new Batterylet(icon_width); target = this->battery; break;
			case 2: this->gps = new Bitmaplet("gps", icon_width); target = this->gps; break;
			}

			this->master->insert(target,
				cell_x + label_offset * 0.5F, cell_y + cell_height * 0.5F,
				GraphletAlignment::CC);
		}

		{ // load yacht
			this->decorator->fill_cell_extent(3, &cell_x, &cell_y, &cell_width, &cell_height);

			float yacht_x = application_fit_size(1447.0F);
			float yacht_width = cell_x + (cell_width - application_fit_size(48.0F)) - yacht_x;

			this->yacht = new Bitmaplet("skeleton", yacht_width);
			this->master->insert(this->yacht,
				yacht_x, cell_y + cell_height * 0.5F,
				GraphletAlignment::LC);
		}
	}

// never deletes these graphlets mannually
private:
	Bitmaplet* yacht;
	Bitmaplet* gps;
	FuelTanklet* fueltank;
	Batterylet* battery;
		
private:
	CanvasTextFormat^ fonts[2];
	Statusbar* master;
	CellDecorator* decorator;
};

/*************************************************************************************************/
Statusbar::Statusbar() : Planet(":statusbar:") {
	float cell_y = 10.0F;
	float cell_width = 382.0F;
	float cell_height = 200.0F;
	float cell_gapsize = 10.0F;
	float yacht_x = 1387.0F;
	Rect boxes[] = {
		make_fit_cell((cell_width + cell_gapsize) * 0.0F + cell_gapsize, cell_y , cell_width, cell_height),
		make_fit_cell((cell_width + cell_gapsize) * 1.0F + cell_gapsize, cell_y, cell_width, cell_height),
		make_fit_cell((cell_width + cell_gapsize) * 2.0F + cell_gapsize, cell_y, cell_width, cell_height),
		make_fit_cell(yacht_x, cell_y, screen_width - cell_gapsize - yacht_x, cell_height)
	};

	BackgroundDecorator* bg = new BackgroundDecorator(0x1E1E1E);
	CellDecorator* cells = new CellDecorator(Colours::Background, boxes); // don't mind, it's Visual Studio's fault

	this->console = new StatusPage(this, cells);
	this->set_decorator(new CompositeDecorator(bg, cells));
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
