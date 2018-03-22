#include "page/hp_single.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "turtle.hpp"

#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPSMode { WindowUI = 0, View, Control };

private class HPS : public WarGrey::SCADA::ModbusConfirmation {
public:
	HPS(HPSingle* master) : workbench(master) {}

public:
	void load_pipelines(float stepsize, float width, float height) {
		Turtle* oil_line = new Turtle(stepsize, true);

		oil_line->move_left(2)->move_up(2)->move_right(2)->jump_left(2);
		oil_line->move_up(2)->move_right(2)->jump_left(2);
		oil_line->move_up(2)->move_right(2)->jump_left(2)->move_up(3);
		oil_line->turn_up_left()->move_left(8)->turn_left_up()->move_up(4);
		oil_line->jump_down(4)->turn_down_left()->move_left(14);
		oil_line->jump_right(2)->move_down(2)->move_left(2);
		oil_line->jump_up(2)->jump_right(6)->move_down(4)->move_left(6);
		oil_line->jump_right(6)->move_down(2)->move_left(6);
		oil_line->jump_right(6)->move_down(2)->move_left(6);
		oil_line->jump_right(6)->move_down(2)->move_left(6);

		this->pipelines[1] = new Tracklet(oil_line, 1.5F, Colors::Goldenrod);

		oil_line->move_left(2);

		this->pipelines[0] = new Tracklet(oil_line, 1.5F, Colors::Silver);

		for (size_t i = 0; i < SNIPS_ARITY(this->pipelines); i++) {
			if (this->pipelines[i] != nullptr) {
				this->workbench->insert(this->pipelines[i]);
			}
		}
	}

	void reflow_pipelines(float stepsize, float vinset, float width, float height) {
		float w, h;

		this->pipelines[1]->fill_extent(0.0F, 0.0F, &w, &h);
		this->workbench->move_to(this->pipelines[0], (width - w) * 0.5F - stepsize * 2.0F, (height - h) * 0.5F);
		this->workbench->move_to(this->pipelines[1], (width - w) * 0.5F, (height - h) * 0.5F);
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
	Tracklet* pipelines[2];

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
			console->load_pipelines(this->gridsize, width, height);

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
		console->reflow_pipelines(this->gridsize, vinset, width, height);
	}
}

void HPSingle::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(snip);
}
