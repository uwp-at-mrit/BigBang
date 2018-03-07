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
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPCMode { WindowUI = 0, View, Control };

static TurtleMove filter_moves[] = {
	TurtleMove::HalfDown, TurtleMove::Right, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight, TurtleMove::RightDownLeft,
	TurtleMove::DoubleLeft, TurtleMove::LeftDownRight, TurtleMove::DoubleRight,
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::RightUp,
	TurtleMove::HalfLeft, TurtleMove::Left,
	TurtleMove::QuadrupleUp, TurtleMove::DoubleUp, TurtleMove::HalfUp,
	TurtleMove::HalfRight, TurtleMove::Right, TurtleMove::UpLeft,
	TurtleMove::QuadrupleLeft, TurtleMove::DoubleLeft,
	TurtleMove::Down, TurtleMove::DoubleLeft, TurtleMove::Left
};

static TurtleMove hp_moves[] = {
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::Right,
	TurtleMove::TripleDown, TurtleMove::DoubleDown, TurtleMove::DownLeft,
	TurtleMove::QuadrupleLeft, TurtleMove::QuadrupleLeft, TurtleMove::QuadrupleLeft,
	TurtleMove::LeftUp, TurtleMove::QuadrupleUp, TurtleMove::TripleUp, TurtleMove::UpLeft,
	TurtleMove::QuadrupleLeft, TurtleMove::QuadrupleLeft, TurtleMove::QuadrupleLeft, TurtleMove::QuadrupleLeft,
	TurtleMove::LeftUp, TurtleMove::QuadrupleUp, TurtleMove::QuadrupleUp, TurtleMove::QuadrupleUp,
	TurtleMove::QuadrupleUp, TurtleMove::QuadrupleUp, TurtleMove::UpRight,
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight,
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::Right,
	TurtleMove::RightDown, TurtleMove::TripleDown, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleDown,
	TurtleMove::QuadrupleDown, TurtleMove::QuadrupleDown, TurtleMove::QuadrupleDown, TurtleMove::Down,
	TurtleMove::DownLeft, TurtleMove::TripleLeft, TurtleMove::LeftDown, TurtleMove::DoubleDown
};

static TurtleMove wp_moves[] = {
	TurtleMove::QuadrupleDown, TurtleMove::QuadrupleDown,
	TurtleMove::QuadrupleLeft, TurtleMove::DoubleLeft, TurtleMove::LeftDown,
	TurtleMove::QuadrupleDown, TurtleMove::QuadrupleDown, TurtleMove::DoubleLeft, TurtleMove::DoubleRight,
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight, TurtleMove::QuadrupleRight,
	TurtleMove::QuadrupleRight, TurtleMove::QuadrupleLeft, TurtleMove::TripleLeft,
	TurtleMove::QuadrupleUp, TurtleMove::QuadrupleUp, TurtleMove::UpLeft,
	TurtleMove::QuadrupleLeft, TurtleMove::DoubleLeft
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
		this->usize = 16.0F;

		this->filter_line = new Tracelinelet(MAKE_TURTLE_MOVES(filter_moves), this->usize);
		this->hp_line = new Tracelinelet(MAKE_TURTLE_MOVES(hp_moves), this->usize);
		this->wp_line = new Tracelinelet(MAKE_TURTLE_MOVES(wp_moves), this->usize);

		this->pumps[0] = new Pumplet(32.0F, 0.0);
		this->pumps[1] = new Pumplet(32.0F, 90.0);
		this->pumps[2] = new Pumplet(32.0F, 180.0);
		this->pumps[3] = new Pumplet(32.0F, 270.0);
		this->pumps[4] = new Pumplet(32.0F, -45.0);
		this->pumps[5] = new Pumplet(32.0F, 135.0);

		this->workbench->insert(this->filter_line);
		this->workbench->insert(this->hp_line);
		this->workbench->insert(this->wp_line);

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
		float pump_gapsize = 16.0F;
		float snip_width, snip_height;
		float pump_x, pump_y;

		for (size_t i = 0; i < SNIPS_ARITY(this->pumps); i++) {
			if (this->pumps[i] != nullptr) {
				this->pumps[i]->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
				pump_x = width - snip_width - vinset;
				pump_y = height - vinset - vinset - snip_height - (snip_height + pump_gapsize) * float(i);
				this->workbench->move_to(this->pumps[i], pump_x, pump_y);
			}
		}

		this->hp_line->fill_extent(0.0F, vinset, &snip_width, &snip_height);
		this->workbench->move_to(this->hp_line, (width - snip_width) * 0.05F, (height - snip_height) * 0.15F);

		this->filter_line->fill_extent(0.0F, vinset, &snip_width, &snip_height);
		this->workbench->move_to(this->filter_line, (width - snip_width) * 0.15F, (height - snip_height) * 0.15F);

		this->wp_line->fill_extent(0.0F, vinset, &snip_width, &snip_height);
		this->workbench->move_to(this->wp_line, (width - snip_width) * 0.75F, (height - snip_height) * 0.40F);
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
	}

public:
	void execute(WarGrey::SCADA::Menu cmd, WarGrey::SCADA::ISnip* snip) override {
		syslog(Log::Info, L"%s motor %ld", cmd.ToString()->Data(), snip->id);
	}

// never deletes these snips mannually
private:
	Gaugelet* gauges[7];
	Pumplet* pumps[6];
	Tracelinelet* filter_line;
	Tracelinelet* hp_line;
	Tracelinelet* wp_line;

private:
	HPCWorkbench* workbench;
	float usize;
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

HPCWorkbench::HPCWorkbench(Platform::String^ plc) : Planet(":hpc:") {
	IPlanetDecorator* decorators[] = { new HPCDecorator(system_graytext_brush()), new GridDecorator(16.0F) };
	HPCConsole* console = new HPCConsole(this);
	Syslog* alarm = new Syslog(Log::Debug, "HPC", default_logger());
	
	this->statusline = new Statuslinelet(Log::Debug);
	alarm->append_log_receiver(this->statusline);
	
	this->console = console;
	this->device = new ModbusClient(alarm, plc, console);
	this->cmdmenu = make_start_stop_menu(console);
	this->set_decorator(MAKE_COMPOSE_DECORATOR(decorators));
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
