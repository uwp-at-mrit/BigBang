#include "page/airconditioner.hpp"
#include "configuration.hpp"

#include "graphlet/dashboard/indicatorlet.hpp"
#include "graphlet/dashboard/thermometerlet.hpp"
#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "satellite.hpp"
#include "credit.hpp"
#include "tongue.hpp"
#include "string.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class AC { Bridge, VIP, Salon, Host, Guest, _ };
private enum class ACInfo { mode, t_sea, t_pipe, aux, _, t_room, t_setting };
private enum class ACMode { Breakdown, Heating, Cooling, _ };
private enum class ACStatus { Normal };

private enum class ACOP { Power, Heater, Cooler, Cool, Heat, PlaceHolder, Refresh, _ };

static const size_t ac_count = _N(AC);
static const unsigned int t_step = 8U;
static const double t_min = -30.0;
static const double t_max = 50.0;

static CanvasTextFormat^ caption_font = nullptr;
static CanvasTextFormat^ fore_font = nullptr;
static CanvasTextFormat^ label_font = nullptr;
static DimensionStyle board_style;

static CanvasSolidColorBrush^ label_color = nullptr;

static void prepare_text_formats(IPlanet* master) {
	if (label_color == nullptr) {
		label_color = Colours::make(0x999999);

		caption_font = make_bold_text_format("Microsoft YaHei", master->sketch_to_application_height(33.75F));
		fore_font = make_text_format("Microsoft YaHei", master->sketch_to_application_height(37.50F));
		label_font = make_text_format("Microsoft YaHei", master->sketch_to_application_height(24.79F));

		board_style.number_font = fore_font;
		board_style.unit_font = make_text_format("Microsoft YaHei", master->sketch_to_application_height(30.00F));
		board_style.number_color = Colours::GhostWhite;
		board_style.unit_color = Colours::GhostWhite;
	}
}

private class ACDecorator final : public TableDecorator {
public:
	ACDecorator(float width, float height, float gapsize) : TableDecorator(0x262626U, ac_count, 3U, gapsize) {
		for (ACInfo id = _E0(ACInfo); id < ACInfo::_; id++) {
			this->infos[id] = make_text_layout(speak(":" + id.ToString() + ":"), label_font);
		}
	}

public:
	void draw_after(CanvasDrawingSession^ ds, float Width, float Height) override {
		for (unsigned int idx = 0; idx < cell_count(); idx++) {
			for (ACInfo id = _E0(ACInfo); id < ACInfo::_; id++) {
				this->draw_text_ct(ds, idx, id);
			}
		}
	}

public:
	void fill_info_anchor(unsigned int idx, ACInfo id, float* anchor_x, float* anchor_y) {
		float fx, fy;

		switch (id) {
		case ACInfo::mode:   fx = 0.25F; fy = 0.64F; break;
		case ACInfo::t_sea:  fx = 0.75F; fy = 0.64F; break;
		case ACInfo::t_pipe: fx = 0.25F; fy = 0.86F; break;
		case ACInfo::aux:    fx = 0.75F; fy = 0.86F; break;
		}

		this->fill_cell_anchor(idx, fx, fy, anchor_x, anchor_y);
	}

private:
	void draw_text_ct(CanvasDrawingSession^ ds, unsigned int idx, ACInfo id) {
		float width = this->infos[id]->LayoutBounds.Width;
		float x, y;

		this->fill_info_anchor(idx, id, &x, &y);
		ds->DrawTextLayout(this->infos[id], x - width * 0.5F, y, label_color);
	}

private:
	std::map<ACInfo, CanvasTextLayout^> infos;
};

/*************************************************************************************************/
private class ACBoard final : public PLCConfirmation {
public:
	ACBoard(ACPage* master, ACDecorator* decorator) : master(master), decorator(decorator) {}

public:
	void load() {
		float icon_width = this->master->sketch_to_application_width(64.0F);
		float mode_width = this->master->sketch_to_application_width(46.0F);

		for (AC room = _E0(AC); room < AC::_; room++) {
			this->thermometers[room] = this->master->insert_one(new Thermometerlet(t_min, t_max, icon_width, 0.0F, 3.0F, t_step, label_color));
			this->temperatures[room] = this->master->insert_one(new Dimensionlet(board_style, "<temperature>"));
			this->captions[room] = this->master->insert_one(new Labellet(speak(room), caption_font, Colours::GhostWhite));
			this->modes[room] = this->master->insert_one(new UnionBitmaplet<ACMode>("AirConditioner/mode", mode_width));
			this->Tseas[room] = this->master->insert_one(new Dimensionlet(board_style, "<temperature>"));
			this->Tpipes[room] = this->master->insert_one(new Dimensionlet(board_style, "<temperature>"));
			this->auxes[room] = this->master->insert_one(new Labellet(speak(ACStatus::Normal), fore_font, Colours::GhostWhite));
		}
	}

	void reflow() {
		float cell_x, cell_y, cell_width, cell_height, cell_whalf, label_bottom;
		float thermometer_rx, thermometer_y, mercury_center_y;
		float label_offset = this->master->sketch_to_application_height(24.0F);

		for (AC room = _E0(AC); room < AC::_; room++) {
			unsigned int i = _I(room);

			this->decorator->fill_cell_extent(i, &cell_x, &cell_y, &cell_width, &cell_height);

			cell_whalf = cell_x + cell_width * 0.5F;
			this->master->move_to(this->captions[room], cell_whalf, cell_y + label_offset, GraphletAnchor::CT);
			this->master->fill_graphlet_location(this->captions[room], nullptr, &label_bottom, GraphletAnchor::CB);

			this->thermometers[room]->fill_mercury_extent(0.5F, nullptr, &mercury_center_y);
			this->master->move_to(this->thermometers[room], cell_whalf, label_bottom + label_offset, GraphletAnchor::CT);
			this->master->fill_graphlet_location(this->thermometers[room], &thermometer_rx, &thermometer_y, GraphletAnchor::RT);
			this->master->move_to(this->temperatures[room], thermometer_rx + label_offset, thermometer_y + mercury_center_y, GraphletAnchor::LC);

			this->place_info(this->modes[room], i, ACInfo::mode);
			this->place_info(this->Tseas[room], i, ACInfo::t_sea);
			this->place_info(this->Tpipes[room], i, ACInfo::t_pipe);
			this->place_info(this->auxes[room], i, ACInfo::aux);
		}
	}

public:
	void fill_metrics(AC room, double* temperature, double* t_sea, double* t_pipe) {
		SET_BOX(temperature, this->temperatures[room]->get_value());
		SET_BOX(t_sea, this->Tseas[room]->get_value());
		SET_BOX(t_pipe, this->Tpipes[room]->get_value());
	}

public:
	void on_digital_input_data(uint8* db28, size_t size, Syslog* logger) override {
		size_t db_idx0 = 433;
		size_t db_idx_acc = 8;

		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		for (AC room = _E0(AC); room < AC::_; room++) {
			size_t idx = db_idx0 + db_idx_acc * _I(room);
			unsigned int mask = DI_ref(db28, idx, idx + 6);
			
			switch (mask) {
			case 0b000000: this->modes[room]->set_value(ACMode::Breakdown); break;
			case 0b000001: case 0b000011: this->modes[room]->set_value(ACMode::Cooling); break;
			case 0b000100: case 0b001100: this->modes[room]->set_value(ACMode::Heating); break;
			default: logger->log_message(Log::Warning, L"incorrect AI@%d: 0b%S", idx, binumber(mask).c_str());
			}
		}

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}
	
	void on_analog_input(uint8* db4, size_t size, Syslog* logger) override {
		size_t db_idx_acc = 3;
		
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->set_temperatures(        db4, 101U, db_idx_acc, GraphletAnchor::LC);
		this->set_values(this->Tpipes, db4, 102U, db_idx_acc, GraphletAnchor::CC);
		this->set_values(this->Tseas,  db4, 103U, db_idx_acc, GraphletAnchor::CC);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	void place_info(IGraphlet* g, unsigned int i, ACInfo type) {
		float anchor_x, anchor_y;

		this->decorator->fill_info_anchor(i, type, &anchor_x, &anchor_y);
		this->master->move_to(g, anchor_x, anchor_y, GraphletAnchor::CB);
	}

	void set_values(std::map<AC, Dimensionlet*> dims, uint8* db, size_t idx0, size_t acc, GraphletAnchor anchor = GraphletAnchor::CC) {
		for (AC room = _E0(AC); room < AC::_; room++) {
			dims[room]->set_value(AI_ref(db, idx0 + acc * _I(room)), anchor);
		}
	}

	void set_temperatures(uint8* db, size_t idx0, size_t acc, GraphletAnchor anchor = GraphletAnchor::LC) {
		for (AC room = _E0(AC); room < AC::_; room++) {
			float t = AI_ref(db, idx0 + acc * _I(room)) - 30.0F;

			this->temperatures[room]->set_value(t, anchor);
			this->thermometers[room]->set_value(t);
		}
	}

// never deletes these graphlets mannually
private:
	std::map<AC, Labellet*> captions;
	std::map<AC, Thermometerlet*> thermometers;
	std::map<AC, Dimensionlet*> temperatures;
	std::map<AC, UnionBitmaplet<ACMode>*> modes;
	std::map<AC, Dimensionlet*> Tseas;
	std::map<AC, Dimensionlet*> Tpipes;
	std::map<AC, Labellet*> auxes;
		
private:
	ACPage* master;
	ACDecorator* decorator;
};

/*************************************************************************************************/
private class ACSatellite final : public ICreditSatellite<ACBoard, AC>, public PLCConfirmation {
public:
	ACSatellite(ACBoard* board, Platform::String^ caption, float bar_height = 64.0F)
		: ICreditSatellite(default_logging_level, board, caption) {
		float width = bar_height * _F(ACOP::_);
		float height = width * 0.618F;
		Rect bg[] = { Rect(0.0F, bar_height, width, height) };

		this->decorator = new CellDecorator(0x383838, bg, 0.0F);
		this->append_decorator(this->decorator);
	}

public:
	void fill_satellite_extent(float* width, float* height) override {
		float y, cell_height;

		this->decorator->fill_cell_extent(0, nullptr, &y, width, &cell_height);
		(*height) = cell_height + y * 2.0F;
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		ACInfo infos[] = { ACInfo::t_setting, ACInfo::t_room, ACInfo::t_pipe, ACInfo::t_sea };
		ICanvasBrush^ style_color = make_solid_brush(0xB3ED00);
		DimensionStyle satellite_style, setting_style;
		float indicator_scale = 0.75F;
		float icon_scale = 0.80F;
		float bar_height, cell_height;

		satellite_style.number_font = board_style.number_font;
		satellite_style.unit_font = board_style.unit_font;
		satellite_style.number_color = Colours::GhostWhite;
		satellite_style.unit_color = Colours::GhostWhite;

		setting_style.number_font = board_style.unit_font;
		setting_style.unit_font = board_style.unit_font;
		setting_style.number_color = style_color;
		setting_style.unit_color = style_color;

		this->decorator->fill_cell_extent(0, nullptr, &bar_height, nullptr, &cell_height);

		this->caption = this->insert_one(new Labellet(AC::_.ToString(), caption_font, style_color));
		this->indicator = this->insert_one(new Indicatorlet(t_min, t_max, cell_height * indicator_scale, indicator_thickness));
		
		for (unsigned idx = 0; idx < sizeof(infos) / sizeof(ACInfo); idx++) {
			ACInfo id = infos[idx];
			Platform::String^ label = speak(":" + id.ToString() + ":");

			if (id == ACInfo::t_setting) {
				this->temperatures[id] = this->insert_one(new Dimensionlet(setting_style, "<temperature>", label));
			} else {
				this->labels[id] = this->insert_one(new Labellet(label, label_font, label_color));
				this->temperatures[id] = this->insert_one(new Dimensionlet(satellite_style, "<temperature>"));
			}
		}

		for (ACOP id = _E(ACOP, 0); id < ACOP::_; id++) {
			Platform::String^ dirname = "AirConditioner/" + id.ToString();

			this->handlers[id] = new Credit<OptionBitmaplet, ACOP>(dirname, bar_height * icon_scale);
			this->handlers[id]->id = id;
			
			this->insert(this->handlers[id]);
		}
	}

	void reflow(float width, float height) override {
		float cell_y, icon_grid_y, caption_x;
		float indicator_x, indicator_y, metrics_x, sea_y, pipe_y;
		float icon_grid_width = width / _F(ACOP::_);

		this->decorator->fill_cell_anchor(0, 0.50F, 0.00F, &caption_x, &cell_y);
		this->decorator->fill_cell_anchor(0, 0.32F, 0.50F, &indicator_x, &indicator_y);
		this->decorator->fill_cell_anchor(0, 0.80F, 0.40F, &metrics_x, &sea_y);
		this->decorator->fill_cell_anchor(0, 0.80F, 0.70F, nullptr, &pipe_y);
		icon_grid_y = height - cell_y * 0.5F;

		this->move_to(this->caption, caption_x, height - icon_grid_y, GraphletAnchor::CC);
		this->move_to(this->indicator, indicator_x, indicator_y, GraphletAnchor::CC);
		this->move_to(this->temperatures[ACInfo::t_setting], this->indicator, GraphletAnchor::CB, GraphletAnchor::CC);
		this->move_labels(ACInfo::t_room, indicator_x, indicator_y);
		this->move_labels(ACInfo::t_sea, metrics_x, sea_y);
		this->move_labels(ACInfo::t_pipe, metrics_x, pipe_y);

		for (ACOP id = _E(ACOP, 0); id < ACOP::_; id++) {
			this->move_to(this->handlers[id], (_F(id) + 0.5F) * icon_grid_width, icon_grid_y, GraphletAnchor::CC);
		}
	}

public:
	void on_hover(IGraphlet* g, float local_x, float local_y) override {
		Credit<OptionBitmaplet, ACOP>* button = dynamic_cast<Credit<OptionBitmaplet, ACOP>*>(g);
		
		if (button != nullptr) {
			button->set_value(true);
		}
	}

	void on_tap(IGraphlet* g, float local_x, float local_y) override {
		Credit<OptionBitmaplet, ACOP>* button = dynamic_cast<Credit<OptionBitmaplet, ACOP>*>(g);

		if (button != nullptr) {
			this->get_logger()->log_message(Log::Info, button->id.ToString());
		}
	}

	void on_goodbye(IGraphlet* g, float local_x, float local_y) override {
		Credit<OptionBitmaplet, ACOP>* button = dynamic_cast<Credit<OptionBitmaplet, ACOP>*>(g);

		if (button != nullptr) {
			button->set_value(false);
		}
	}

public:
	bool available() override {
		return (this->surface_ready() && this->shown());
	}

	void on_analog_input(uint8* db4, size_t size, Syslog* logger) override {
		this->enter_critical_section();
		this->begin_update_sequence();

		this->pull_metrics();

		this->end_update_sequence();
		this->leave_critical_section();
	}

protected:
	void on_channel_changed(AC room) override {
		this->caption->set_text(speak(room), GraphletAnchor::CC);
		this->pull_metrics();
	}

private:
	void move_labels(ACInfo id, float x, float y) {
		this->move_to(this->labels[id], x, y, GraphletAnchor::CT);
		this->move_to(this->temperatures[id], x, y, GraphletAnchor::CB);
	}

	void pull_metrics() {
		double t, t_sea, t_pipe;

		this->master->fill_metrics(this->channel, &t, &t_sea, &t_pipe);
		
		this->indicator->set_value(t);
		this->temperatures[ACInfo::t_room]->set_value(t);
		this->temperatures[ACInfo::t_sea]->set_value(t_sea);
		this->temperatures[ACInfo::t_pipe]->set_value(t_pipe);
	}

	// never deletes these graphlets mannually
private:
	Labellet* caption;
	Indicatorlet* indicator;
	std::map<ACInfo, Labellet*> labels;
	std::map<ACInfo, Dimensionlet*> temperatures;
	std::map<ACOP, Credit<OptionBitmaplet, ACOP>*> handlers;

private:
	CellDecorator* decorator;
};

/*************************************************************************************************/
ACPage::ACPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

ACPage::~ACPage() {
	if (this->satellite != nullptr) {
		delete this->satellite;
	}

	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void ACPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	prepare_text_formats(this);
	
	if (this->dashboard == nullptr) {
		ACDecorator* cells = new ACDecorator(width, height, this->sketch_to_application_width(2.0F));
		ACBoard* ac = new ACBoard(this, cells);
		ACSatellite* acs = new ACSatellite(ac, this->name() + "#Satellite");
	
		this->append_decorator(cells);
		ac->load();

		this->dashboard = ac;
		this->satellite = acs;
		this->decorator = cells;
		
		this->device->append_confirmation_receiver(ac);
		this->device->append_confirmation_receiver(acs);
	}
}

void ACPage::reflow(float width, float height) {
	ACBoard* ac = dynamic_cast<ACBoard*>(this->dashboard);

	if (ac != nullptr) {
		ac->reflow();
	}
}

void ACPage::on_tap(IGraphlet* g, float local_x, float local_y) {
	if (g != nullptr) {
#ifdef _DEBUG
		Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
	} else {
		int cell_idx = this->decorator->find_cell(local_x, local_y);

		if (cell_idx >= 0) {
			ACSatellite* satellite = static_cast<ACSatellite*>(this->satellite);

			satellite->switch_channel(_E(AC, cell_idx));
			satellite->show();
		}
	}
}
