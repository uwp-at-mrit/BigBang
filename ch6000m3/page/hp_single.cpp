#include "page/hp_single.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "turtle.idl"

#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPSMode { WindowUI = 0, View, Control };

private enum class HPS {
	A, B, C, D, E, F, G, H, I, J, K, Y, F001, SQ1, SQ2, SQ3,
	SQa, SQb, SQc, SQd, SQe, SQf, SQg, SQh, SQi, SQj, SQk, SQy,
	_,
	a, b, c, d, e, f, g, h, k
};

private class HPSConsole : public WarGrey::SCADA::ModbusConfirmation {
public:
	HPSConsole(HPSingle* master) : workbench(master) {}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HPS>* pump_station = new Turtle<HPS>(gridsize, true, HPS::SQ1);

		pump_station->move_down(4)->turn_down_right()->move_right(10)->turn_right_down();
		pump_station->move_down(4, HPS::f)->move_right(3, HPS::SQf)->move_right(6, HPS::F)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::c)->move_right(3, HPS::SQc)->move_right(6, HPS::C)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::d)->move_right(3, HPS::SQd)->move_right(6, HPS::D)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::e)->move_right(3, HPS::SQe)->move_right(6, HPS::E)->move_right(3)->move_up(30);
		pump_station->turn_up_left()->move_left(26)->turn_left_down()->move_down(1.5F, HPS::F001)->move_down(1.5F);
		pump_station->jump_up(3)->turn_up_left()->move_left(20)->turn_left_down()->move_down(17);
		pump_station->move_down(4, HPS::a)->move_right(3, HPS::A)->move_right(6, HPS::SQa)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::b)->move_right(3, HPS::B)->move_right(6, HPS::SQb)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::g)->move_right(3, HPS::G)->move_right(6, HPS::SQg)->move_right(3)->jump_back();
		pump_station->move_down(3, HPS::h)->move_right(3, HPS::H)->move_right(6, HPS::SQh)->move_right(3)->move_up(16);
		pump_station->turn_up_right()->move_right(8)->turn_right_up()->move_up(1, HPS::SQ2);
		pump_station->jump_right(8, HPS::SQ3)->move_down()->turn_down_right()->move_right(8, HPS::k);
		pump_station->move_right(3, HPS::SQk)->move_right(3)->turn_right_up()->move_up(8, HPS::K)->move_up(5)->jump_back();
		pump_station->move_up(5, HPS::SQy)->move_up(4, HPS::Y)->move_up(5);

		this->stations[0] = new Tracklet<HPS>(pump_station, 1.5F, Colors::Goldenrod);

		for (size_t i = 0; i < SNIPS_ARITY(this->stations); i++) {
			if (this->stations[i] != nullptr) {
				this->workbench->insert(this->stations[i]);
			}
		}
	}

	void load_pump_elements(float width, float height, float gridsize) {
		double left_degree = 180.0;
		double right_degree = 0.0;
		double up_degree = -90.0;

		this->pumps[0] = this->make_pumplet(gridsize, left_degree, HPS::A);
		this->pumps[1] = this->make_pumplet(gridsize, left_degree, HPS::B);
		this->pumps[2] = this->make_pumplet(gridsize, left_degree, HPS::G);
		this->pumps[3] = this->make_pumplet(gridsize, left_degree, HPS::H);

		this->pumps[4] = this->make_pumplet(gridsize, right_degree, HPS::F);
		this->pumps[5] = this->make_pumplet(gridsize, right_degree, HPS::C);
		this->pumps[6] = this->make_pumplet(gridsize, right_degree, HPS::D);
		this->pumps[7] = this->make_pumplet(gridsize, right_degree, HPS::E);

		this->pumps[8] = this->make_pumplet(gridsize, up_degree, HPS::Y);
		this->pumps[9] = this->make_pumplet(gridsize, up_degree, HPS::K);

		for (size_t i = 0; i < SNIPS_ARITY(this->pumps); i++) {
			if (this->pumps[i] != nullptr) {
				HPS id = static_cast<HPS>(this->pumps[i]->id);

				this->pump_labels[i] = this->make_labellet(id.ToString(), id);
			}
		}
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float station_width, station_height;

		this->stations[0]->fill_extent(0.0F, 0.0F, &station_width, &station_height);
		this->workbench->move_to(this->stations[0], (width - station_width) * 0.5F, (height - station_height) * 0.5F);
	}
	
	void reflow_pump_elements(float width, float height, float gridsize, float vinset) {
		float station_x, station_y;

		this->workbench->fill_snip_location(this->stations[0], &station_x, &station_y);

		for (size_t i = 0; i < SNIPS_ARITY(this->pumps); i++) {
			this->place_id_element(this->pumps[i], station_x, station_y);
			this->place_id_element(this->pump_labels[i], station_x + gridsize, station_y + gridsize);
		}
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime) override {
		device->read_input_registers(0, 16);
	}

	void on_input_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {
		float Mpa = 0.0F;
		
		//this->workbench->enter_critical_section();
		
		//for (size_t i = 1; i < SNIPS_ARITY(this->gauges); i++) {
		//	float mpa = float(register_values[i]) * 0.1F;

		//	Mpa = Mpa + mpa;
		//	this->gauges[i]->set_scale(mpa);
		//}

		//this->gauges[0]->set_scale(Mpa);

		//this->workbench->leave_critical_section();
	}

	void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Error, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	}

private:
	Pumplet* make_pumplet(float radius, double degree, HPS id) {
		Pumplet* pump = new Pumplet(radius, degree);

		pump->id = static_cast<long>(id);
		this->workbench->insert(pump);

		return pump;
	}

	Labellet* make_labellet(Platform::String^ caption, HPS id = HPS::_) {
		Labellet* label = new Labellet(caption);

		label->id = static_cast<long>(id);
		this->workbench->insert(label);

		return label;
	}

	void place_id_element(ISnip* snip, float dx, float dy) {
		if (snip != nullptr) {
			float x, y;

			this->stations[0]->fill_anchor_location(static_cast<HPS>(snip->id), &x, &y);
			this->workbench->move_to(snip, x + dx, y + dy, SnipCenterPoint::CC);
		}
	}

// never deletes these snips mannually
private:
	Tracklet<HPS>* stations[2];
	Pumplet* pumps[12];
	Labellet* pump_labels[12];

private:
	HPSingle* workbench;
};

private class HPSDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
public:
	HPSDecorator(ICanvasBrush^ brush) : brush(brush) {}

public:
	void draw_after_snip(ISnip* self, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) override {
		if (x == 0.0) {
			if (y == 0.0) { // statusbar's bottomline 
				ds->DrawLine(0, height, width, height, this->brush, 2.0F);
			} else if (self == this->statusline) { // statusline's topline
				ds->DrawLine(0, y, width, y, this->brush, 2.0F);
			} else { // avoid dynamic_cast every time.
				auto maybe_statusline = dynamic_cast<Statuslinelet*>(self);
				
				if (maybe_statusline != nullptr) {
					this->statusline = maybe_statusline;
					ds->DrawLine(0, y, width, y, this->brush, 2.0F);
				}
			}
		}
	}

protected:
	~HPSDecorator() noexcept {}

private:
	ICanvasBrush^ brush;
	Statuslinelet* statusline;
};

HPSingle::HPSingle(Platform::String^ plc) : Planet(":hps:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HPS");
	HPSConsole* console = new HPSConsole(this);

	this->console = console; 
	this->device = new ModbusClient(alarm, plc, this->console);
	this->gridsize = statusbar_height();
}

HPSingle::~HPSingle() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}
}

void HPSingle::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load snips
			this->change_mode(HPSMode::View);
			console->load_pump_station(width, height, this->gridsize);
			console->load_pump_elements(width, height, this->gridsize);

			this->change_mode(HPSMode::Control);

			this->change_mode(HPSMode::WindowUI);
			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			GridDecorator* grid = new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset);
			IPlanetDecorator* decorators[] = { new HPSDecorator(system_graytext_brush()), grid };

			this->set_decorator(MAKE_COMPOSE_DECORATOR(decorators));

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void HPSingle::reflow(float width, float height) {
	auto console = dynamic_cast<HPSConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HPSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, SnipCenterPoint::LB);
		
		this->change_mode(HPSMode::Control);

		this->change_mode(HPSMode::View);
		console->reflow_pump_station(width, height, this->gridsize, vinset);
		console->reflow_pump_elements(width, height, this->gridsize, vinset);
	}
}

void HPSingle::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(snip);
}
