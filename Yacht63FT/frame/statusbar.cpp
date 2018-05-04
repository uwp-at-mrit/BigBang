#include "frame/statusbar.hpp"
#include "decorator/background.hpp"
#include "decorator/cell.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/batterylet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "text.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum Status { OilTank, Battery, GPS_E, GPS_N };

/*************************************************************************************************/
private class StatusBoard final : public PLCConfirmation, public ISystemStatusListener {
public:
	~StatusBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	StatusBoard(Statusbar* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->fonts[0] = make_text_format("Microsoft YaHei", application_fit_size(30.0F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(41.27F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		Platform::String^ captions[] = { ":oiltank:", ":battery:", ":gps:" };
		IGraphlet* target = nullptr;
		float cell_x, cell_y, cell_width, cell_height, icon_bottom, px, py;
		float icon_width = application_fit_size(110.0F) * 0.618F;
		float label_xoffset = application_fit_size(screen_status_label_xoff);
		float label_yoffset = application_fit_size(screen_status_label_yoff);
		float parameter_yoffset = application_fit_size(screen_status_parameter_yoff);

		for (unsigned int i = Status::OilTank; i <= Status::GPS_E; i++) {
			this->decorator->fill_cell_extent(i, &cell_x, &cell_y);

			{ // load label
				this->labels[i] = new Labellet(speak(captions[i]), this->fonts[0], screen_status_label_color);
				this->master->insert(this->labels[i],
					cell_x + label_xoffset, cell_y + label_yoffset,
					GraphletAlignment::LT);
			}

			{ // load parameters
				px = cell_x + label_xoffset;
				py = cell_y + parameter_yoffset;

				if (i < Status::GPS_E) {
					this->parameters[i] = new Labellet("%", this->fonts[1], screen_status_parameter_color);
					this->master->insert(this->parameters[i], px, py, GraphletAlignment::LT);
				} else {
					this->parameters[Status::GPS_E] = new Labellet("E:", this->fonts[1], screen_status_parameter_color);
					this->parameters[Status::GPS_N] = new Labellet("N:", this->fonts[1], screen_status_parameter_color);
					this->master->insert(this->parameters[2], px, py, GraphletAlignment::LB);
					this->master->insert(this->parameters[3], px, py, GraphletAlignment::LT);
				}
			}

			{ // load icon
				TextExtent ts = get_text_extent("%", this->fonts[1]);
				this->master->fill_graphlet_location(this->parameters[0], nullptr, &icon_bottom, GraphletAlignment::LB);

				switch (i) {
				case Status::OilTank: this->oiltank = new FuelTanklet(icon_width, -1.5714F); target = this->oiltank; break;
				case Status::Battery: this->battery = new Batterylet(icon_width, -1.5714F); target = this->battery; break;
				case Status::GPS_E: this->gps = new Bitmaplet("gps", icon_width * 1.78F); target = this->gps; break;
				}

				px = cell_x + label_xoffset * 0.5F;
				py = icon_bottom - ts.bspace;
				this->master->insert(target, px, py, GraphletAlignment::CB);
			}
		}

		{ // load alarm status and yacht
			this->decorator->fill_cell_extent(3, &cell_x, &cell_y, &cell_width, &cell_height);
			py = cell_y + cell_height * 0.5F;

			this->alarm = new BitmapBooleanlet("Alarm", application_fit_size(screen_status_alarm_width));
			this->yacht = new Bitmaplet("skeleton", application_fit_size(screen_status_yacht_width));

			this->master->insert(this->alarm, application_fit_size(screen_status_alarm_x), py, GraphletAlignment::LC);
			this->master->insert(this->yacht, application_fit_size(screen_status_yacht_x), py, GraphletAlignment::LC);

			this->alarm->set_value(true);
		}
	}

public:
	void on_battery_capacity_changed(float flcapacity) override { // NOTE: Batterylet manages capacity own its own.
		float percentage = std::roundf(flcapacity * 100.0F);
		
		this->parameters[Status::Battery]->set_text(percentage.ToString() + "%");

		{
			this->oiltank->set_value(flcapacity);
			this->parameters[Status::OilTank]->set_text(percentage.ToString() + "%");
		}
	}

// never deletes these graphlets mannually
private:
	BitmapBooleanlet* alarm;
	Bitmaplet* yacht;
	Bitmaplet* gps;
	FuelTanklet* oiltank;
	Batterylet* battery;
	Labellet* labels[Status::GPS_E + 1];
	Labellet* parameters[Status::GPS_N + 1];
		
private:
	CanvasTextFormat^ fonts[2];
	Statusbar* master;
	CellDecorator* decorator;
};

/*************************************************************************************************/
Statusbar::Statusbar(IMRMaster* device) : Planet(":statusbar:"), device(device) {}

Statusbar::~Statusbar() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void Statusbar::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		float cell_width = application_fit_size(385.0F);
		float cell_height = application_fit_size(200.0F);
		float cell_gapsize = application_fit_size(10.0F);
		float cell_y = cell_gapsize;
		float yacht_cell_x = application_fit_size(1388.0F);

		Rect boxes[] = {
			Rect((cell_width + cell_gapsize) * 0.0F + cell_gapsize, cell_y, cell_width, cell_height),
			Rect((cell_width + cell_gapsize) * 1.0F + cell_gapsize, cell_y, cell_width, cell_height),
			Rect((cell_width + cell_gapsize) * 2.0F + cell_gapsize, cell_y, cell_width, cell_height),
			Rect(yacht_cell_x, cell_y, width - cell_gapsize - yacht_cell_x, cell_height)
		};

		BackgroundDecorator* bg = new BackgroundDecorator(0x1E1E1E);
		CellDecorator* cells = new CellDecorator(Colours::Background, boxes); // don't mind, it's Visual Studio's fault
		StatusBoard* status = new StatusBoard(this, cells);
		
		status->load_and_flow(width, height);
		register_system_status_listener(status);

		this->dashboard = status;
		this->set_decorator(new CompositeDecorator(bg, cells));
		this->device->append_confirmation_receiver(status);
	}
}

void Statusbar::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
