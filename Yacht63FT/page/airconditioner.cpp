#include "page/airconditioner.hpp"
#include "decorator/background.hpp"
#include "decorator/cell.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/thermometerlet.hpp"
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
private enum class ACInfo { mode, t_sea, t_pipe, aux, _ };
private enum class ACMode { Breakdown, Heating, Refrigeration, _ };

static size_t cell_count = 7;
static unsigned int decorator_text_color = 0x666666;

private class ACDecorator final : public CellDecorator {
public:
	ACDecorator(float width, float height) : CellDecorator(0x1E1E1E, width, height, cell_count, 4, application_fit_size(2.0F)) {
		auto font = make_text_format("Microsoft YaHei", application_fit_size(24.79F));

		this->color = Colours::make(decorator_text_color);

		for (ACInfo id = ACInfo::mode; id < ACInfo::_; id++) {
			this->infos[id] = make_text_layout(speak(":" + id.ToString() + ":"), font);
		}
	}

public:
	void draw_after(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int i = 0; i < cell_count; i++) {
			this->draw_text_ct(ds, i, ACInfo::mode);
			this->draw_text_ct(ds, i, ACInfo::t_sea);
			this->draw_text_ct(ds, i, ACInfo::t_pipe);
			this->draw_text_ct(ds, i, ACInfo::aux);
		}
	}

public:
	void fill_info_anchor(unsigned int idx, ACInfo id, float* anchor_x, float* anchor_y) {
		float x, y, width, height;

		this->fill_cell_extent(idx, &x, &y, &width, &height);
	
		switch (id) {
		case ACInfo::mode:   SET_VALUES(anchor_x, x + width * 0.25F, anchor_y, y + height * 0.64F); break;
		case ACInfo::t_sea:  SET_VALUES(anchor_x, x + width * 0.75F, anchor_y, y + height * 0.64F); break;
		case ACInfo::t_pipe: SET_VALUES(anchor_x, x + width * 0.25F, anchor_y, y + height * 0.86F); break;
		case ACInfo::aux:    SET_VALUES(anchor_x, x + width * 0.75F, anchor_y, y + height * 0.86F); break;
		}
	}

private:
	void draw_text_ct(CanvasDrawingSession^ ds, unsigned int idx, ACInfo id) {
		float x, y;
		float width = this->infos[id]->LayoutBounds.Width;

		this->fill_info_anchor(idx, id, &x, &y);
		ds->DrawTextLayout(this->infos[id], x - width * 0.5F, y, this->color);
	}

private:
	ICanvasBrush^ color;
	std::map<ACInfo, CanvasTextLayout^> infos;
};

/*************************************************************************************************/
private class ACBoard final : public PLCConfirmation {
public:
	~ACBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	ACBoard(AirConditioner* master, ACDecorator* decorator) : master(master), decorator(decorator) {
		this->fonts[0] = make_text_format("Microsoft YaHei", application_fit_size(33.75F));
		this->fonts[1] = make_text_format("Microsoft YaHei", application_fit_size(37.50F));
		this->fonts[2] = make_text_format("Microsoft YaHei", application_fit_size(30.00F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		Platform::String^ T = speak("celsius");
		float cell_x, cell_y, cell_width, cell_height, cell_whalf;
		float label_yoffset = application_fit_size(25.0);
		float icon_yoffset = application_fit_size(81.0);
		float icon_width = application_fit_size(80.0F);
		float mode_width = application_fit_size(46.0F);

		for (AC room = AC::Bridge; room <= AC::_; room++) {
			unsigned int i = static_cast<unsigned int>(room);

			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);

			cell_whalf = cell_x + cell_width * 0.5F;
			this->thermometers[room] = new Thermometerlet(icon_width);
			this->captions[room] = new Labellet(speak(room.ToString()), this->fonts[0], Colours::GhostWhite);
			this->modes[room] = new BitmapStatelet<ACMode>("ACMode", mode_width);
			this->Tseas[room] = new ScaleTextlet(T, "", "", this->fonts[1], this->fonts[2], Colours::GhostWhite, Colours::GhostWhite);
			this->Tpipes[room] = new ScaleTextlet(T, "", "", this->fonts[1], this->fonts[2], Colours::GhostWhite, Colours::GhostWhite);
			this->auxes[room] = new Labellet("Normal", this->fonts[1], Colours::GhostWhite);

			this->master->insert(this->captions[room], cell_whalf, cell_y + label_yoffset, GraphletAlignment::CT);
			this->master->insert(this->thermometers[room], cell_whalf, cell_y + icon_yoffset, GraphletAlignment::CT);

			this->load_info(this->modes[room], i, ACInfo::mode);
			this->load_info(this->Tseas[room], i, ACInfo::t_sea);
			this->load_info(this->Tpipes[room], i, ACInfo::t_pipe);
			this->load_info(this->auxes[room], i, ACInfo::aux);
		}
	}

private:
	void load_info(IGraphlet* g, unsigned int i, ACInfo type) {
		float anchor_x, anchor_y;

		this->decorator->fill_info_anchor(i, type, &anchor_x, &anchor_y);
		this->master->insert(g, anchor_x, anchor_y, GraphletAlignment::CB);
	}

// never deletes these graphlets mannually
private:
	std::map<AC, Thermometerlet*> thermometers;
	std::map<AC, Labellet*> captions;
	std::map<AC, BitmapStatelet<ACMode>*> modes;
	std::map<AC, ScaleTextlet*> Tseas;
	std::map<AC, ScaleTextlet*> Tpipes;
	std::map<AC, Labellet*> auxes;
		
private:
	CanvasTextFormat^ fonts[3];
	AirConditioner* master;
	ACDecorator* decorator;
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
		ACDecorator* cells = new ACDecorator(width, height);
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
