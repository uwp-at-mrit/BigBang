#include "page/hydraulic_pressure.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "turtle.hpp"

#include "decorator/border.hpp"
#include "decorator/grid.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HPCMode { WindowUI = 0, View, Control };

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

	void load_workline(float stepsize, float width, float height) {
		this->stepsize = stepsize;

		{ // load pipelines
			Turtle* filter_turtle = new Turtle(this->stepsize);
			Turtle* hp_turtle = new Turtle(this->stepsize);
			Turtle* wp_turtle = new Turtle(this->stepsize);

			filter_turtle->move_down(0.5F)->move_right()->turn_right_down_left();
			filter_turtle->move_left(2)->turn_left_down_right()->move_right(2)->turn_right_down_left();
			filter_turtle->move_left(2)->turn_left_down_right()->move_right(2)->turn_right_down_left();
			filter_turtle->move_left(2)->turn_left_down_right()->move_right(10);
			filter_turtle->turn_right_up()->move_up(6.5F)->turn_up_left()->move_left(9);
			filter_turtle->move_down();

			hp_turtle->move_right(6.5F, 3)->move_right(6.5F)->move_down(4.5F);
			hp_turtle->move_left(6.5F, 6)->move_left(6.5F)->move_up(6)->turn_up_left()->move_left(16);
			hp_turtle->turn_left_up()->move_up(18)->turn_up_right()->move_right(29);
			hp_turtle->turn_right_down()->move_down(2)->move_right(4)->move_down(16);
			hp_turtle->turn_down_left()->move_left(3)->turn_left_down()->move_down(1.5F);

			wp_turtle->move_down(8)->move_left(6)->turn_left_down()->move_down(4, 1)->move_down(4);
			wp_turtle->move_left(2)->move_right(22)->move_left(7)->move_up(4, 2)->move_up(4);
			wp_turtle->turn_up_left()->move_left(6);

			this->filter_line = new Tracklet(filter_turtle);
			this->hp_line = new Tracklet(hp_turtle);
			this->wp_line = new Tracklet(wp_turtle);

			this->workbench->insert(this->hp_line);
			this->workbench->insert(this->wp_line);
			this->workbench->insert(this->filter_line);
		}

		{ // load pumps
			float pump_radius = this->stepsize;

			for (size_t i = 0; i < SNIPS_ARITY(this->hpumps); i++) {
				this->hpumps[i] = new Pumplet(pump_radius, 180.0);
				this->workbench->insert(this->hpumps[i]);
			}

			for (size_t i = 0; i < SNIPS_ARITY(this->wpumps); i++) {
				this->wpumps[i] = new Pumplet(pump_radius, -90.0);
				this->workbench->insert(this->wpumps[i]);
			}
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
		float hp_x = this->stepsize * 3.0F;
		float hp_y = this->stepsize * 1.0F + vinset;
		float wp_x = this->stepsize * 48.0F;
		float wp_y = this->stepsize * 12.0F + vinset;

		{ // reflow pipelines
			this->workbench->move_to(this->hp_line, hp_x, hp_y);
			this->workbench->move_to(this->filter_line, this->stepsize * 10.0F, this->stepsize * 3.0F + vinset);
			this->workbench->move_to(this->wp_line, wp_x, wp_y);
		}

		{ // reflow pumps
			float pump_x, pump_y;

			this->hp_line->fill_anchor_location(3, &pump_x, &pump_y);
			this->workbench->move_to(this->hpumps[0], hp_x + pump_x, hp_y + pump_y, SnipCenterPoint::CC);
			this->hp_line->fill_anchor_location(6, &pump_x, &pump_y);
			this->workbench->move_to(this->hpumps[1], hp_x + pump_x, hp_y + pump_y, SnipCenterPoint::CC);

			this->wp_line->fill_anchor_location(1, &pump_x, &pump_y);
			this->workbench->move_to(this->wpumps[0], wp_x + pump_x, wp_y + pump_y, SnipCenterPoint::CC);
			this->wp_line->fill_anchor_location(2, &pump_x, &pump_y);
			this->workbench->move_to(this->wpumps[1], wp_x + pump_x, wp_y + pump_y, SnipCenterPoint::CC);
		}
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

	void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Error, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	}

public:
	void execute(WarGrey::SCADA::Menu cmd, WarGrey::SCADA::ISnip* snip) override {
		syslog(Log::Info, L"%s motor %ld", cmd.ToString()->Data(), snip->id);
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
	HPCWorkbench* workbench;
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

HPCWorkbench::HPCWorkbench(Platform::String^ plc) : Planet(":hpc:") {
	Syslog* alarm = make_system_logger(default_logging_level, "HPC");
	HPCConsole* console = new HPCConsole(this);

	this->console = console;
	this->cmdmenu = make_start_stop_menu(console);
	this->device = new ModbusClient(alarm, plc, this->console);
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
		float vinset = statusbar_height();
		float stepsize = 16.0F;

		{ // load snips
			this->change_mode(HPCMode::View);
			console->load_gauges(width, height);
			console->load_workline(stepsize, width, height);

			this->change_mode(HPCMode::Control);

			this->change_mode(HPCMode::WindowUI);
			this->statusline = new Statuslinelet(Log::Debug);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->shift = new Togglet(false, "control_mode", "view_mode", -6.18F);
			this->insert(this->statusbar);
			this->insert(this->statusline);
			this->insert(this->shift);
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
	} else if (!this->shift->checked()) {
		Pumplet* pump = dynamic_cast<Pumplet*>(snip);

		if (pump != nullptr) {
			// this->set_selected(snip);
			
			// TODO: protect the menu from showing out of screen
			this->cmdmenu->show_for(pump, local_x, local_y, 2.0F, 2.0F);
			this->set_caret_owner(pump);

			// this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		} else {
			// this->no_selected();
		}
	}
}
