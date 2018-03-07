#include "hydraulic_pressure.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"

#include "decorator/border.hpp"
#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static inline Scalelet* load_scalelet(IPlanet* master, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript) {
	Scalelet* scale = new Scalelet(unit, label, subscript);
	master->insert(scale);

	return scale;
}

static inline void connect_pipes(IPlanet* master, IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx = 0.5, double fy = 0.5) {
    pipe_connecting_position(prev, pipe, x, y, fx, fy);
    master->move_to(pipe, (*x), (*y));
}

// WARNING: order matters, Desulphurizer, Cleaner and Mooney are also anchors for water pipes 
private enum HPCMode { WindowUI = 0, View, Control };

static TurtleMove filter_moves[] = {
	TurtleMove::HalfDown, TurtleMove::Right, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight,
	TurtleMove::DoubleRight, TurtleMove::DoubleRight,
	TurtleMove::DoubleRight, TurtleMove::DoubleRight, TurtleMove::RightUp,
	TurtleMove::HalfLeft, TurtleMove::Left,
	TurtleMove::DoubleUp, TurtleMove::DoubleUp, TurtleMove::DoubleUp, TurtleMove::HalfUp,
	TurtleMove::HalfRight, TurtleMove::Right, TurtleMove::UpLeft,
	TurtleMove::DoubleLeft, TurtleMove::DoubleLeft, TurtleMove::DoubleLeft,
	TurtleMove::Down, TurtleMove::DoubleLeft, TurtleMove::Left
};

private class HPCConsole : public WarGrey::SCADA::ModbusConfirmation, public WarGrey::SCADA::IMenuCommand<WarGrey::SCADA::Menu> {
public:
	HPCConsole(HPCWorkbench* master) : workbench(master) {}

public:
	void load_gauges(float width, float height) {
		Platform::String^ caption_suffix = speak("n_oilmpa");

		this->gauges[0] = new Gaugelet(speak("oilmpa"), 40);
		this->workbench->insert(this->gauges[0]);

		for (size_t i = 1; i < SNIPS_ARITY(this->gauges); i++) {
			this->gauges[i] = new Gaugelet(i.ToString() + caption_suffix, 40);
			this->workbench->insert(this->gauges[i]);
		}
	}

	void load_workline(float width, float height) {
		this->filter_line = new Tracelinelet(MAKE_TURTLE_MOVES(filter_moves), 16.0F);
		this->pumps[0] = new Pumplet(32.0F, 0.0);
		this->pumps[1] = new Pumplet(32.0F, 90.0);
		this->pumps[2] = new Pumplet(32.0F, 180.0);
		this->pumps[3] = new Pumplet(32.0F, 270.0);
		this->pumps[4] = new Pumplet(32.0F, -45.0);
		this->pumps[5] = new Pumplet(32.0F, 135.0);

		this->workbench->insert(this->filter_line);
		for (size_t i = 0; i < SNIPS_ARITY(this->pumps); i++) {
			this->workbench->insert(this->pumps[i]);
		}
	}

public:
	void reflow_gauges(float vinset, float width, float height) {
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

	void reflow_workline(float vinset, float width, float height) {
		float pump_gapsize = 32.0F;
		float pump_x = 0.0F;
		float pump_y = vinset + vinset;
		float snip_width, snip_height;

		for (size_t i = 0; i < SNIPS_ARITY(this->pumps); i++) {
			if (this->pumps[i] != nullptr) {
				this->workbench->move_to(this->pumps[i], pump_x, pump_y);
				this->pumps[i]->fill_extent(pump_x, pump_y, &snip_width);
				pump_x += (snip_width + pump_gapsize);
			}
		}

		this->filter_line->fill_extent(0.0F, vinset, &snip_width, &snip_height);
		this->workbench->move_to(this->filter_line, (width - snip_width) * 0.5F, (height - snip_height) * 0.5F);
	}

public:
	void on_scheduled_request(IModbusClient* device, long long count, long long interval, long long uptime, bool is_slow) override {
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

	void on_exception(uint16 transaction, uint8 function_code, uint16 maybe_address, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Error, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	};

public:
	void execute(WarGrey::SCADA::Menu cmd, WarGrey::SCADA::ISnip* snip) override {
		syslog(Log::Info, L"%s motor %ld", cmd.ToString()->Data(), snip->id);
	}

private:
	void load_scales_t(Scalelet* scales[], size_t arity) {
		for (size_t i = 0; i < arity; i++) {
			scales[i] = load_scalelet(this->workbench, "celsius", "temperature", nullptr);
		}
	}

	void load_scales_pt(Scalelet* scales[], size_t arity) {
		for (size_t i = 0; i < arity; i++) {
			scales[i + arity * 0] = load_scalelet(this->workbench, "bar", "pressure", nullptr);
			scales[i + arity * 1] = load_scalelet(this->workbench, "celsius", "temperature", nullptr);
		}
	}

	void load_scales_ptt(Scalelet* scales[], size_t arity) {
		for (size_t i = 0; i < arity; i++) {
			scales[i + arity * 0] = load_scalelet(this->workbench, "bar", "pressure", nullptr);
			scales[i + arity * 1] = load_scalelet(this->workbench, "celsius", "temperature", "inside");
			scales[i + arity * 2] = load_scalelet(this->workbench, "celsius", "temperature", "outside");
		}
	}

	void move_scales_pt(Scalelet* scales[], size_t arity, size_t idx, float x, float y, float scale_height) {
		this->workbench->move_to(scales[idx + arity * 0], x, y);
		this->workbench->move_to(scales[idx + arity * 1], x, y + scale_height);
	}

	void move_scales_ptt(Scalelet* scales[], size_t arity, float x0, float y_ascent, float scale_height, float hgap, float vgap) {
		float y_descent = y_ascent + vgap;

		for (size_t idx = 0; idx < arity; idx++) {
			float x = x0 + hgap * float(idx);

			this->workbench->move_to(scales[idx + arity * 0], x, y_ascent - scale_height);
			this->workbench->move_to(scales[idx + arity * 1], x, y_descent + scale_height * 0.0F);
			this->workbench->move_to(scales[idx + arity * 2], x, y_descent + scale_height * 1.0F);
		}
	}

	void update_scales_pt(Scalelet* scales[], size_t i, uint16* modbus, size_t idx, float ratio) {
		scales[i + 0]->set_scale(float(modbus[idx + 0]) * ratio);
		scales[i + 1]->set_scale(float(modbus[idx + 1]) * ratio);
	}

	void update_scales_ptt(Scalelet* scales[], size_t arity, uint16* modbus, size_t addr, float ratio) {
		for (size_t i = 0; i < arity; i++) {
			size_t idx = addr + i * 3;
		
			scales[i + arity * 0]->set_scale(float(modbus[idx + 0] * ratio));
			scales[i + arity * 1]->set_scale(float(modbus[idx + 1] * ratio));
			scales[i + arity * 2]->set_scale(float(modbus[idx + 2] * ratio));
		}
	}

// never deletes these snips mannually
private:
	Gaugelet* gauges[7];
	Pumplet* pumps[6];
	Tracelinelet* filter_line;

private:
	HPCWorkbench* workbench;
};

private class HPCDecorator : public virtual WarGrey::SCADA::GridDecorator {
public:
	HPCDecorator(ICanvasBrush^ brush) : GridDecorator(16.0F), brush(brush) {}

public:
	void draw_after_snip(ISnip* self, CanvasDrawingSession^ ds, float x, float y, float width, float height) override {
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

		GridDecorator::draw_after_snip(self, ds, x, y, width, height);
	}

private:
	ICanvasBrush^ brush;
	Statuslinelet* statusline;
};

HPCWorkbench::HPCWorkbench(Platform::String^ plc) : Planet(":hpc:") {
	HPCConsole* console = new HPCConsole(this);
	Syslog* alarm = new Syslog(Log::Debug, "HPC", default_logger());
	
	this->statusline = new Statuslinelet(Log::Debug);
	alarm->append_log_receiver(this->statusline);
	
	this->console = console;
	this->device = new ModbusClient(alarm, plc, console);
	this->cmdmenu = make_start_stop_menu(console);
	this->set_decorator(new HPCDecorator(system_graytext_brush()));
}

HPCWorkbench::~HPCWorkbench() {
	if (this->device != nullptr) {
		delete this->device;
	}

	if (this->console != nullptr) {
		delete this->console;
	}

	if (this->cmdmenu != nullptr) {
		delete this->cmdmenu;
	}
}

void HPCWorkbench::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<HPCConsole*>(this->console);

	if (console != nullptr) {
		this->change_mode(HPCMode::View);
		console->load_gauges(width, height);
		console->load_workline(width, height);
		
		this->change_mode(HPCMode::Control);
		
		this->change_mode(HPCMode::WindowUI);
		this->statusbar = new Statusbarlet(this->name(), this->device);
		this->shift = new Togglet(false, "control_mode", "view_mode", -6.18F);
		this->insert(this->statusbar);
		this->insert(this->statusline);
		this->insert(this->shift);
	}
}

void HPCWorkbench::reflow(float width, float height) {
	auto console = dynamic_cast<HPCConsole*>(this->console);
	
	if (console != nullptr) {
		float vinset, toggle_width;

		this->change_mode(HPCMode::WindowUI);
		this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &vinset);
		this->shift->fill_extent(0.0F, 0.0F, &toggle_width, nullptr);
		this->move_to(this->statusline, 0.0F, height - vinset);
		this->move_to(this->shift, width - toggle_width - vinset, vinset + vinset);

		this->change_mode(HPCMode::Control);

		this->change_mode(HPCMode::View);
		console->reflow_workline(vinset, width, height);
		console->reflow_gauges(vinset, width, height);
	}
}

void HPCWorkbench::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	if (snip == this->shift) {
		this->shift->toggle();
		this->change_mode(this->shift->checked() ? HPCMode::Control : HPCMode::View);
	} else if (this->shift->checked()) {
		// Motorlet* motor = dynamic_cast<Motorlet*>(snip);

		// if (motor != nullptr) {
			// TODO: protect the menu from showing out of screen
			//  this->cmdmenu->show_for(motor, local_x, local_y, 2.0F, 2.0F);
			//  this->set_caret_owner(motor);

			//  this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		// }
	} else {
		if ((snip != this->statusbar) && (snip != this->statusline)) {
			this->set_selected(snip);
		}
	}
}
