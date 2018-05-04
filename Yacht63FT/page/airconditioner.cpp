#include "page/airconditioner.hpp"
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

private enum class AC { Bridge, VIP, Kitchen, Central, Salon, Host, Guest, _ };

/*************************************************************************************************/
private class ACBoard final : public PLCConfirmation {
public:
	~ACBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	ACBoard(AirConditioner* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->fonts[0] = make_text_format("Microsoft YaHei", application_fit_size(33.75F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(37.50F));
		this->fonts[2] = make_text_format("Microsoft YaHei", application_fit_size(30.00F));
		this->fonts[3] = make_text_format("Microsoft YaHei", application_fit_size(24.79F));

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

		for (AC room = AC::Bridge; room <= AC::_; room++) {
			unsigned int i = static_cast<unsigned int>(room);
			this->decorator->fill_cell_extent(i, &cell_x, &cell_y);

			{ // load label
				this->labels[i] = new Labellet(speak(room.ToString()), this->fonts[0], screen_status_label_color);
				this->master->insert(this->labels[i],
					cell_x + label_xoffset, cell_y + label_yoffset,
					GraphletAlignment::LT);
			}
		}
	}

// never deletes these graphlets mannually
private:
	BitmapBooleanlet* alarm;
	Bitmaplet* yacht;
	Bitmaplet* gps;
	FuelTanklet* oiltank;
	Batterylet* battery;
	Labellet* labels[static_cast<unsigned int>(AC::_)];
		
private:
	CanvasTextFormat^ fonts[4];
	AirConditioner* master;
	CellDecorator* decorator;
};

/*************************************************************************************************/
AirConditioner::AirConditioner(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

AirConditioner::~AirConditioner() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void AirConditioner::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		CellDecorator* cells = new CellDecorator(0x1E1E1E, width, height, 7, 4, application_fit_size(2.0F));
		ACBoard* ac = new ACBoard(this, cells);

		ac->load_and_flow(width, height);

		this->dashboard = ac;
		this->set_decorator(cells);
		this->device->append_confirmation_receiver(ac);
	}
}

void AirConditioner::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
