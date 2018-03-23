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

private enum class HPSlot { _,
	A, B, C, D, E, F, G, H, I, J, K, Y, F001, SQ1, SQ2, SQ3,
	SQa, SQb, SQc, SQd, SQe, SQf, SQg, SQh, SQi, SQj, SQk, SQy,
};

private class HPS : public WarGrey::SCADA::ModbusConfirmation {
public:
	HPS(HPSingle* master) : workbench(master) {}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HPSlot>* pump_station = new Turtle<HPSlot>(gridsize, true, HPSlot::SQ1);

		pump_station->move_down(4)->turn_down_right()->move_right(10)->turn_right_down();
		pump_station->move_down(4)->move_right(3, HPSlot::SQf)->move_right(6, HPSlot::F)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::SQc)->move_right(6, HPSlot::C)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::SQd)->move_right(6, HPSlot::D)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::SQe)->move_right(6, HPSlot::E)->move_right(3)->move_up(30);
		pump_station->turn_up_left()->move_left(26)->turn_left_down()->move_down(1.5F, HPSlot::F001)->move_down(1.5F);
		pump_station->jump_up(3)->turn_up_left()->move_left(20)->turn_left_down()->move_down(17);
		pump_station->move_down(4)->move_right(3, HPSlot::A)->move_right(6, HPSlot::SQa)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::B)->move_right(6, HPSlot::SQb)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::G)->move_right(6, HPSlot::SQg)->move_right(3)->jump_left(12);
		pump_station->move_down(3)->move_right(3, HPSlot::H)->move_right(6, HPSlot::SQh)->move_right(3)->move_up(16);
		pump_station->turn_up_right()->move_right(8)->turn_right_up()->move_up(1, HPSlot::SQ2);
		pump_station->jump_right(8, HPSlot::SQ3)->move_down()->turn_down_right()->move_right(8);
		pump_station->move_right(3, HPSlot::SQk)->move_right(3)->turn_right_up();
		pump_station->move_up(8, HPSlot::K)->move_up(5)->jump_left(7);
		pump_station->move_down(5, HPSlot::Y)->move_down(4, HPSlot::SQy)->move_down(5);

		this->stations[0] = new Tracklet<HPSlot>(pump_station, 1.5F, Colors::Goldenrod);

		for (size_t i = 0; i < SNIPS_ARITY(this->stations); i++) {
			if (this->stations[i] != nullptr) {
				this->workbench->insert(this->stations[i]);
			}
		}
	}

	void reflow_pump_station(float width, float height, float stepsize, float vinset) {
		float station_width, station_height;

		this->stations[0]->fill_extent(0.0F, 0.0F, &station_width, &station_height);
		this->workbench->move_to(this->stations[0], (width - station_width) * 0.5F, (height - station_height) * 0.5F);
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime) override {
		device->read_input_registers(0, 128);
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

// never deletes these snips mannually
private:
	Tracklet<HPSlot>* stations[2];

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
	HPS* console = new HPS(this);

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
	auto console = dynamic_cast<HPS*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load snips
			this->change_mode(HPSMode::View);
			console->load_pump_station(width, height, this->gridsize);

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
	auto console = dynamic_cast<HPS*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HPSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, SnipCenterPoint::LB);
		
		this->change_mode(HPSMode::Control);

		this->change_mode(HPSMode::View);
		console->reflow_pump_station(width, height, this->gridsize, vinset);
	}
}

void HPSingle::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(snip);
}
