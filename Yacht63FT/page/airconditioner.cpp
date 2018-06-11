#include "page/airconditioner.hpp"
#include "decorator/cell.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/thermometerlet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class AC { Bridge, VIP, Central, Salon, Host, Guest, _ };
private enum class ACInfo { mode, t_sea, t_pipe, aux, _ };
private enum class ACMode { Breakdown, Heating, Refrigeration, _ };
private enum class ACStatus { Normal };

static size_t cell_count = static_cast<size_t>(AC::_);
static unsigned int decorator_text_color = 0x666666;

private class ACDecorator final : public CellDecorator {
public:
	ACDecorator(float width, float height) : CellDecorator(0x1E1E1E, width, height, cell_count, 3, design_to_application_width(2.0F)) {
		auto font = make_text_format("Microsoft YaHei", design_to_application_height(24.79F));

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

	ACBoard(ACPage* master, ACDecorator* decorator) : master(master), decorator(decorator) {
		this->fonts[0] = make_text_format("Microsoft YaHei", design_to_application_height(33.75F));
		this->fonts[1] = make_text_format("Microsoft YaHei", design_to_application_height(37.50F));
		this->fonts[2] = make_text_format("Microsoft YaHei", design_to_application_height(30.00F));

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		float cell_x, cell_y, cell_width, cell_height, cell_whalf, label_bottom;
		float label_yoffset = design_to_application_height(screen_caption_yoff);
		float icon_width = design_to_application_width(64.0F);
		float mode_width = design_to_application_width(46.0F);

		for (AC room = AC::Bridge; room < AC::_; room++) {
			unsigned int i = static_cast<unsigned int>(room);

			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);

			cell_whalf = cell_x + cell_width * 0.5F;
			this->thermometers[room] = new Thermometerlet(icon_width, 0.0F, Colours::make(decorator_text_color));
			this->captions[room] = new Labellet(speak(room.ToString()), this->fonts[0], Colours::GhostWhite);
			this->modes[room] = new UnionBitmaplet<ACMode>("ACMode", mode_width);
			this->Tseas[room] = new Dimensionlet("<temperature>", this->fonts[1], this->fonts[2], Colours::GhostWhite);
			this->Tpipes[room] = new Dimensionlet("<temperature>", this->fonts[1], this->fonts[2], Colours::GhostWhite);
			this->auxes[room] = new Labellet(speak(ACStatus::Normal.ToString()), this->fonts[1], Colours::GhostWhite);

			this->master->insert(this->captions[room], cell_whalf, cell_y + label_yoffset, GraphletAlignment::CT);

			this->master->fill_graphlet_location(this->captions[room], nullptr, &label_bottom, GraphletAlignment::CB);
			this->master->insert(this->thermometers[room], cell_whalf, label_bottom + label_yoffset, GraphletAlignment::CT);

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
	std::map<AC, UnionBitmaplet<ACMode>*> modes;
	std::map<AC, Dimensionlet*> Tseas;
	std::map<AC, Dimensionlet*> Tpipes;
	std::map<AC, Labellet*> auxes;
		
private:
	CanvasTextFormat^ fonts[3];
	ACPage* master;
	ACDecorator* decorator;
};

/*************************************************************************************************/
ACPage::ACPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

ACPage::~ACPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void ACPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		ACDecorator* cells = new ACDecorator(width, height);
		ACBoard* ac = new ACBoard(this, cells);

		ac->load_and_flow(width, height);

		this->dashboard = ac;
		this->set_decorator(cells);
		this->device->append_confirmation_receiver(ac);
	}
}

void ACPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
