#include "page/airconditioning.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "turtle.hpp"

#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPCMode { WindowUI = 0, View, Control };

private class AirController : public WarGrey::SCADA::ModbusConfirmation {
public:
	AirController(AirConditioning* master) : workbench(master) {}

public:
	void load_controllers(float stepsize, float width, float height) {
		Platform::String^ caption_suffix = speak("n_oilmpa");

		this->gauges[0] = new Gaugelet(speak("oilmpa"), 40);
		this->workbench->insert(this->gauges[0]);

		for (size_t i = 1; i < SNIPS_ARITY(this->gauges); i++) {
			this->gauges[i] = new Gaugelet(i.ToString() + caption_suffix, 40);
			this->workbench->insert(this->gauges[i]);
		}
	}

public:
	void reflow_controllers(float vinset, float width, float height) {
		float gauge_gapsize = 32.0F;
		float gauge_x = 0.0F;
		float gauge_y = vinset;
		float snip_width, snip_height;

		this->gauges[0]->fill_extent(gauge_x, gauge_y, nullptr, &snip_height);
		gauge_y = height - snip_height - vinset - vinset;
		for (size_t i = 1; i < SNIPS_ARITY(this->gauges); i++) {
			this->workbench->move_to(this->gauges[i], gauge_x, gauge_y);
			this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
			gauge_x += (snip_width + gauge_gapsize);
		}

		this->workbench->move_to(this->gauges[0], gauge_x, gauge_y);
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime) override {
		device->read_input_registers(0, SNIPS_ARITY(this->gauges));
	}

	void on_input_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {
		float Mpa = 0.0F;
		
		this->workbench->enter_critical_section();
		
		for (size_t i = 1; i < SNIPS_ARITY(this->gauges); i++) {
			float mpa = float(register_values[i]) * 0.1F;

			Mpa = Mpa + mpa;
			this->gauges[i]->set_scale(mpa);
		}

		this->gauges[0]->set_scale(Mpa);

		this->workbench->leave_critical_section();
	}

	void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Error, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	}

// never deletes these snips mannually
private:
	Gaugelet* gauges[7];
	Pumplet* hpumps[2];
	Pumplet* wpumps[2];
	Tracklet* filter_line;
	Tracklet* hp_line;
	Tracklet* wp_line;

private:
	AirConditioning* workbench;
	float stepsize;
};

private class HPCDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
public:
	HPCDecorator(ICanvasBrush^ brush) : brush(brush) {}

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
	~HPCDecorator() noexcept {}

private:
	ICanvasBrush^ brush;
	Statuslinelet* statusline;
};

AirConditioning::AirConditioning(Platform::String^ plc) : Planet(":hpc:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HPC");
	AirController* console = new AirController(this);

	this->console = console;
	this->device = new ModbusClient(alarm, plc, this->console);
}

AirConditioning::~AirConditioning() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}
}

void AirConditioning::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<AirController*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();
		float stepsize = 16.0F;

		{ // load snips
			this->change_mode(HPCMode::View);
			console->load_controllers(stepsize, width, height);

			this->change_mode(HPCMode::Control);

			this->change_mode(HPCMode::WindowUI);
			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			GridDecorator* grid = new GridDecorator(stepsize, 0.0F, 0.0F, vinset);
			IPlanetDecorator* decorators[] = { new HPCDecorator(system_graytext_brush()), grid };

			this->set_decorator(MAKE_COMPOSE_DECORATOR(decorators));

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void AirConditioning::reflow(float width, float height) {
	auto console = dynamic_cast<AirController*>(this->console);
	
	if (console != nullptr) {
		float vinset;

		this->change_mode(HPCMode::WindowUI);
		this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &vinset);
		this->move_to(this->statusline, 0.0F, height - vinset);
		
		this->change_mode(HPCMode::Control);

		this->change_mode(HPCMode::View);
		console->reflow_controllers(vinset, width, height);
	}
}

void AirConditioning::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(snip);
}
