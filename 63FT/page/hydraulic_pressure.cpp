#include "hydraulic_pressure.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"

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

private class HPCConsole : public WarGrey::SCADA::ModbusConfirmation, public WarGrey::SCADA::IMenuCommand<WarGrey::SCADA::Menu> {
public:
	HPCConsole(HPCWorkbench* master) : workbench(master) {};

public:
	void load_gauges(float width, float height) {
		Platform::String^ caption = "#" + speak("oilmpa");

		for (size_t i = 0; i < SNIPS_ARITY(this->gauges) && this->gauges[i] != nullptr; i++) {
			this->gauges[i] = new Gaugelet((i + 1).ToString() + caption, 40);
			this->workbench->insert(this->gauges[i]);
		}
	}

public:
	void reflow_gauges(float vinset, float width, float height) {
		float gauge_gapsize = 32.0F;
		float gauge_x = 0.0F;
		float gauge_y = 0.0F;
		float snip_width, snip_height;

		this->gauges[0]->fill_extent(gauge_x, gauge_y, nullptr, &snip_height);
		gauge_y = height - snip_height - vinset;
		for (size_t i = 0; i < SNIPS_ARITY(this->gauges); i++) {
			this->workbench->move_to(this->gauges[i], gauge_x, gauge_y);
			this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
			gauge_x += (snip_width + gauge_gapsize);
		}
	}

public:
	void execute(WarGrey::SCADA::Menu cmd, WarGrey::SCADA::ISnip* snip) override {
		auto motor = static_cast<Motorlet*>(snip);

		syslog(Log::Info, L"%s motor %ld", cmd.ToString()->Data(), motor->id);
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
	Gaugelet* gauges[6];

private:
	HPCWorkbench* workbench;
};

private class HPCDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
public:
	HPCDecorator(ICanvasBrush^ brush) : IPlanetDecorator(), brush(brush) {};

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
	}

private:
	ICanvasBrush^ brush;
	Statuslinelet* statusline;
};

HPCWorkbench::HPCWorkbench(Platform::String^ plc) : Planet(speak(":hpc:")), device(plc) {
	HPCConsole* console = new HPCConsole(this);

	this->console = console;
	this->cmdmenu = make_start_stop_menu(console);
	this->set_decorator(new HPCDecorator(system_graytext_brush()));
}

HPCWorkbench::~HPCWorkbench() {
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
		
		this->change_mode(HPCMode::Control);
		
		this->change_mode(HPCMode::WindowUI);
		this->statusline = new Statuslinelet(Log::Debug);
		this->statusbar = new Statusbarlet(this->caption);
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
		console->reflow_gauges(vinset, width, height);
	}
}

void HPCWorkbench::on_tap(ISnip* snip, float local_x, float local_y, bool shifted, bool ctrled) {
	if (snip == this->shift) {
		this->shift->toggle();
		this->change_mode(this->shift->checked() ? HPCMode::Control : HPCMode::View);
	} else if (this->shift->checked()) {
		Motorlet* motor = dynamic_cast<Motorlet*>(snip);

		if (motor != nullptr) {
			// TODO: protect the menu from showing out of screen
			//this->cmdmenu->show_for(motor, local_x, local_y, 2.0F, 2.0F);
			this->set_caret_owner(motor);

			this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		}
	}
}
