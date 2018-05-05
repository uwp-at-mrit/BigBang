#include "page/light.hpp"
#include "decorator/background.hpp"
#include "decorator/cell.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum class L { Bridge, VIP, Salon, Host, Guest, Kitchen, _ };

static size_t cell_count = static_cast<size_t>(L::_);

/*************************************************************************************************/
private class LightBoard final : public PLCConfirmation {
public:
	~LightBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	LightBoard(Light* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->font = make_text_format("Microsoft YaHei", application_fit_size(33.75F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		float cell_x, cell_y, cell_width, cell_height, cell_whalf, cell_top;
		float label_yoffset = application_fit_size(16.0);
		
		for (L room = L::Bridge; room < L::_; room++) {
			unsigned int i = static_cast<unsigned int>(room);

			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);
			cell_whalf = cell_x + cell_width * 0.5F;

			this->captions[room] = new Labellet(speak(room.ToString()), this->font, Colours::GhostWhite);
			this->master->insert(this->captions[room], cell_whalf, cell_y + label_yoffset, GraphletAlignment::CT);

			this->master->fill_graphlet_location(this->captions[room], nullptr, &cell_top, GraphletAlignment::LB);
			this->lights[room] = new BitmapBooleanlet("Light", cell_width, cell_height - label_yoffset);

			if ((room != L::Bridge) && (room != L::Salon)) {
				this->curtains[room] = new BitmapBooleanlet("Curtain", cell_width);
				this->master->insert(this->curtains[room], cell_whalf, cell_top + label_yoffset, GraphletAlignment::CT);
			}

			this->master->insert(this->lights[room], cell_whalf, cell_top + label_yoffset, GraphletAlignment::CT);
		}
	}

// never deletes these graphlets mannually
private:
	std::map<L, Labellet*> captions;
	std::map<L, BitmapBooleanlet*> lights;
	std::map<L, BitmapBooleanlet*> curtains;
		
private:
	CanvasTextFormat^ font;
	CellDecorator* decorator;
	Light* master;
};

/*************************************************************************************************/
Light::Light(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

Light::~Light() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void Light::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		CellDecorator* cells = new CellDecorator(0x1E1E1E, width, height, cell_count, 3, application_fit_size(2.0F));
		LightBoard* lb = new LightBoard(this, cells);

		lb->load_and_flow(width, height);

		this->dashboard = lb;
		this->set_decorator(cells);
		this->device->append_confirmation_receiver(lb);
	}
}

void Light::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
