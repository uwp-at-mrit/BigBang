#include "page/fire.hpp"
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

private enum class F {
	Bridge, BowDeck, SternDeck, Kitchen, Salon, Host,
	Guest, Passage, Staircase, VIP, Engine, BowCabin,
	_
};

static size_t cell_count = static_cast<size_t>(F::_);

/*************************************************************************************************/
private class FireBoard final : public PLCConfirmation {
public:
	~FireBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	FireBoard(Fire* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->font = make_text_format("Microsoft YaHei", application_fit_size(33.75F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		float cell_x, cell_y, cell_width, cell_height, cell_whalf, cell_top;
		float face_width, face_height;
		float hmargin = application_fit_size(screen_caption_yoff);
		float vmargin = application_fit_size(screen_caption_yoff);
		
		for (F room = F::Bridge; room < F::_; room++) {
			unsigned int i = static_cast<unsigned int>(room);

			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);
			cell_whalf = cell_x + cell_width * 0.5F;

			this->captions[room] = new Labellet(speak(room.ToString()), this->font, Colours::GhostWhite);
			this->master->insert(this->captions[room], cell_whalf, cell_y + vmargin, GraphletAlignment::CT);

			this->master->fill_graphlet_location(this->captions[room], nullptr, &cell_top, GraphletAlignment::LB);
			cell_top += vmargin;
			face_width = cell_width - hmargin * 2.0F;
			face_height = cell_y + cell_height - cell_top - vmargin;

			this->status[room] = new OptionBitmaplet("Fire", face_width, face_height);
			this->status[room]->set_value(true);
			
			this->master->insert(this->status[room], cell_whalf, cell_top, GraphletAlignment::CT);
		}
	}

// never deletes these graphlets mannually
private:
	std::map<F, Labellet*> captions;
	std::map<F, Bitmaplet*> faces;
	std::map<F, OptionBitmaplet*> status;
		
private:
	CanvasTextFormat^ font;
	CellDecorator* decorator;
	Fire* master;
};

/*************************************************************************************************/
Fire::Fire(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

Fire::~Fire() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void Fire::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		CellDecorator* cells = new CellDecorator(0x1E1E1E, width, height, cell_count, 6, application_fit_size(2.0F));
		FireBoard* fb = new FireBoard(this, cells);

		fb->load_and_flow(width, height);

		this->dashboard = fb;
		this->set_decorator(cells);
		this->device->append_confirmation_receiver(fb);
	}
}

void Fire::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	// this override does nothing but disabling the default behaviours
}
