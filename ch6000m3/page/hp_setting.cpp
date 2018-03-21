#include "page/hp_setting.hpp"
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

private enum HPCMode { WindowUI = 0, View, Control };

private class AirController : public WarGrey::SCADA::ModbusConfirmation {
public:
	AirController(AirConditioning* master) : workbench(master) {}

public:
	void load_icons(float stepsize, float width, float height) {
		this->stepsize = stepsize;

		for (size_t i = 0; i < SNIPS_ARITY(this->icons); i++) {
			if (this->icons[i] != nullptr) {
				this->workbench->insert(this->icons[i]);
			}
		}
	}

public:
	void reflow_icons(float vinset, float width, float height) {
		size_t ic = SNIPS_ARITY(this->icons);
		float gapsize = 32.0F;
		float icon_x = 0.0F;
		float icon_y = vinset;
		float icon_width, icon_height;

		this->icons[0]->fill_extent(icon_x, icon_y, &icon_width, &icon_height);
		icon_x = (width - icon_width * float(ic) - gapsize * float(ic - 1)) * 0.5F;
		icon_y = vinset + icon_height;

		for (size_t i = 0; i < ic; i++) {
			this->workbench->move_to(this->icons[i], icon_x, icon_y);
			icon_x += (icon_width + gapsize);
		}
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime) override {
		device->read_input_registers(0, SNIPS_ARITY(this->icons));
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
	Iconlet* icons[5];

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

AirConditioning::AirConditioning(Platform::String^ plc) : Planet(":ac:") {
	Syslog* alarm = make_system_logger(default_logging_level, "AC");
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
		float stepsize = vinset;

		{ // load snips
			this->change_mode(HPCMode::View);
			console->load_icons(stepsize, width, height);

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
		float vinset = statusbar_height();

		this->change_mode(HPCMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, SnipCenterPoint::LB);
		
		this->change_mode(HPCMode::Control);

		this->change_mode(HPCMode::View);
		//console->reflow_icons(vinset, width, height);
	}
}

void AirConditioning::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	// this->set_caret_owner(snip);
}
