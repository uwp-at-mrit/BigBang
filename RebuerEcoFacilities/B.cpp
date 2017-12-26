#include "B.hpp"
#include "tongue.hpp"
#include "system.hpp"

#include "text.hpp"
#include "paint.hpp"

#include "decorator/grid.hpp"
#include "decorator/pipeline.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

#define SNIPS_ARITY(a) sizeof(a) / sizeof(Snip*)

static inline Motorlet* load_motorlet(IUniverse* master, float width, double degree = 0.0) {
	Motorlet* motor = new Motorlet(width);
	master->insert(motor, degree);

	return motor;
}

static inline Gaugelet* load_gaugelet(IUniverse* master, Platform::String^ caption, int A, int RPM) {
	Gaugelet* gauge = new Gaugelet(caption, A, RPM);
	master->insert(gauge);

	return gauge;
}

static inline Scalelet* load_scalelet(IUniverse* master, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript) {
	Scalelet* scale = new Scalelet(unit, label, subscript);
	master->insert(scale);

	return scale;
}

static inline LPipelet* load_pipelet(IUniverse* master, float length, float thickness, double hue, double saturation, double light, double highlight) {
	LPipelet* pipe = new LPipelet(length, 0.0F, thickness, hue, saturation, light, highlight);
	master->insert(pipe);

	return pipe;
}

static inline Liquidlet* load_water_pipe(IUniverse* master, float length, double degrees = 0.0) {
	Liquidlet* waterpipe = new Liquidlet(length, ArrowPosition::End, 209.60, 1.000, 0.559);
	master->insert(waterpipe, degrees);

	return waterpipe;
}

static inline Liquidlet* load_oil_pipe(IUniverse* master, float length, double degrees = 0.0) {
	Liquidlet* oilpipe = new Liquidlet(length, ArrowPosition::Start, 38.825, 1.000, 0.500);
	master->insert(oilpipe, degrees);

	return oilpipe;
}

static inline void connect_pipes(IUniverse* master, IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx = 0.5, double fy = 0.5) {
    pipe_connecting_position(prev, pipe, x, y, fx, fy);
    master->move_to(pipe, (*x), (*y));
}

void connect_motor(IUniverse* master, IMotorSnip* pipe, Motorlet* motor, Scalelet* scale, float x, float y, double fx = 1.0, double fy = 1.0) {
	// TODO: there must be a more elegant way to deal with rotated motors
	float motor_width, motor_height, scale_width, scale_height, yoff;
	Rect mport = pipe->get_motor_port();

	motor->fill_extent(0.0F, 0.0F, &motor_width, &motor_height);
	scale->fill_extent(0.0F, 0.0F, &scale_width, &scale_height);
	master->fill_snip_bound(motor, nullptr, &yoff, nullptr, nullptr);

	x = x + mport.X + (mport.Width - motor_width) * float(fx);
	y = y + mport.Y + (mport.Height - motor_height + yoff) * float(fy);
	master->move_to(motor, x, y);

	if (yoff == 0.0F) {
		x = x + (motor_width - scale_width) * 0.3F;
		master->move_to(scale, x, y + motor_height);
	} else {
		x = x + (motor_width - scale_width) * 0.5F;
		master->move_to(scale, x, y - scale_height + yoff);
	}
}

private enum B { Master = 0, Cleaner, Slave, Funnel, Count }; // WARNING: order matters

private class BConsole : public ModbusConfirmation {
public:
	BConsole(BSegment* master, Platform::String^ caption) : bench(master), caption(caption) {};

public:
	void load_window_frame(float width, float height) {
		this->statusbar = new Statuslet(this->caption);
		this->bench->insert(this->statusbar);
	}

	void load_icons(float width, float height) {
		this->icons[0] = new StorageTanklet(80.0F);

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->bench->insert(this->icons[i]);
		}
	}

	void load_gauges(float width, float height) {
		this->gauges[B::Master] = load_gaugelet(this->bench, "mastermotor", 100, 100);
		this->gauges[B::Funnel] = load_gaugelet(this->bench, "feedingmotor", 200, 100);
		this->gauges[B::Cleaner] = load_gaugelet(this->bench, "cleanmotor", 10, 20);
		this->gauges[B::Slave] = load_gaugelet(this->bench, "slavemotor", 200, 100);
	}

	void load_workline(float width, float height) {
		float pipe_length = 128.0F;
		float pipe_thickness = 32.0F;
		float master_height = 128.0F;
		float funnel_width = 42.0F;
		float slave_height = 80.0F;

		{ // load water and oil pipes
			this->water_pipes[0] = load_water_pipe(this->bench, pipe_length, 0.0);
			for (unsigned char i = 1; i < SNIPS_ARITY(this->water_pipes); i++) {
				this->water_pipes[i] = load_water_pipe(this->bench, pipe_length, 90.0);
			}

			this->oil_pipes[0] = load_oil_pipe(this->bench, pipe_length);
			for (unsigned char i = 1; i < SNIPS_ARITY(this->oil_pipes); i++) {
				this->oil_pipes[i] = load_oil_pipe(this->bench, pipe_length, 90.0);
			}
		}

		this->master = new LScrewlet(pipe_length, master_height, pipe_thickness);
		this->slave = new LScrewlet(pipe_length, slave_height, pipe_thickness);
		this->cleaner = new GlueCleanerlet(pipe_length, master_height, pipe_thickness);
		this->funnel = new Funnellet(funnel_width, 0.0F, 120.0, 0.7, 0.3, 0.84);
		this->vibrator = new Vibratorlet(pipe_thickness * 1.618F);

		this->bench->insert(this->master);
		this->bench->insert(this->funnel);

		for (size_t i = 0; i < SNIPS_ARITY(this->pipes_1st); i++) {
			this->pipes_1st[i] = load_pipelet(this->bench, pipe_length, pipe_thickness, nan("Silver"), 0.000, 0.512, 0.753);
			this->Pps[i] = load_scalelet(this->bench, "bar", "pressure", nullptr);
			this->Tpis[i] = load_scalelet(this->bench, "celsius", "temperature", "inside");
			this->Tpos[i] = load_scalelet(this->bench, "celsius", "temperature", "outside");
		}

		this->bench->insert(this->cleaner);
		this->bench->insert(this->slave);

		for (size_t i = 0; i < SNIPS_ARITY(this->Pss); i++) {
			this->Pss[i] = load_scalelet(this->bench, "bar", "pressure", nullptr);
			this->Tss[i] = load_scalelet(this->bench, "celsius", "temperature", nullptr);
		}

		for (size_t i = 0; i < SNIPS_ARITY(this->pipes_2nd); i++) {
			this->pipes_2nd[i] = load_pipelet(this->bench, pipe_length, pipe_thickness, 120.0, 0.607, 0.339, 0.839);
		}

		this->bench->insert(this->vibrator);

		{ // load motors
			this->motors[B::Funnel] = load_motorlet(this->bench, funnel_width, 90.0);
			this->motors[B::Master] = load_motorlet(this->bench, master_height * 0.85F);
			this->motors[B::Slave] = load_motorlet(this->bench, slave_height * 0.85F);
			this->motors[B::Cleaner] = load_motorlet(this->bench, pipe_thickness, 90.0);

			for (unsigned int i = 0; i < B::Count; i++) {
				this->Tms[i] = load_scalelet(this->bench, "celsius", "temperature", nullptr);
			}
		}
	}

public:
	void reflow_window_frame(float width, float height) {
		this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &this->console_y);
	}

	void reflow_icons(float width, float height) {
		float icon_gapsize = 64.0F;
		float icon_hmax = 0.0F;
		float icon_x = 0.0F;
		float icon_y = this->console_y * 1.5F;
		float icon_width, icon_height;

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &icon_height);
			icon_hmax = max(icon_height, icon_hmax);
		}

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->icons[i]->fill_extent(icon_x, icon_y, &icon_width, &icon_height);
			this->bench->move_to(this->icons[i], icon_x, icon_y + (icon_hmax - icon_height) * 0.5F);
			icon_x += (icon_width + icon_gapsize);
		}
	}

	void reflow_gauges(float width, float height) {
		float gauge_gapsize = 32.0F;
		float gauge_x = 0.0F;
		float gauge_y = 0.0F;
		float snip_width, snip_height;

		this->gauges[0]->fill_extent(gauge_x, gauge_y, nullptr, &snip_height);
		gauge_y = height - snip_height;
		for (unsigned int i = 0; i < SNIPS_ARITY(this->gauges); i++) {
			this->bench->move_to(this->gauges[i], gauge_x, gauge_y);
			this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
			gauge_x += (snip_width + gauge_gapsize);
		}
	}

	void reflow_workline(float width, float height) {
		float pipe_length, pipe_thickness, snip_width, snip_height;
		size_t pcount_1st = SNIPS_ARITY(this->pipes_1st);
		size_t pcount_2nd = SNIPS_ARITY(this->pipes_2nd);

		this->pipes_1st[0]->fill_extent(0.0F, 0.0F, &pipe_length, &pipe_thickness);
		this->funnel->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

		float current_x = pipe_length * 1.618F;
		float current_y = (height - snip_height) * 0.25F;
		this->bench->move_to(this->funnel, current_x, current_y);
		this->move_motor(B::Funnel, this->funnel, current_x, current_y, 0.5, 1.0);

		connect_pipes(this->bench, this->funnel, this->master, &current_x, &current_y, 0.2, 0.5);
		this->move_motor(B::Master, this->master, current_x, current_y);
		connect_pipes(this->bench, this->master, this->pipes_1st[0], &current_x, &current_y);

		for (size_t i = 1; i < pcount_1st; i++) {
			connect_pipes(this->bench, this->pipes_1st[i - 1], this->pipes_1st[i], &current_x, &current_y);
		}

		connect_pipes(this->bench, this->pipes_1st[pcount_1st - 1], this->cleaner, &current_x, &current_y);
		this->move_motor(B::Cleaner, this->cleaner, current_x, current_y, 0.5, 1.0);
		connect_pipes(this->bench, this->cleaner, this->slave, &current_x, &current_y);
		this->move_motor(B::Slave, this->slave, current_x, current_y);
		connect_pipes(this->bench, this->slave, this->pipes_2nd[0], &current_x, &current_y);

		for (size_t i = 1; i < pcount_2nd; i++) {
			connect_pipes(this->bench, this->pipes_2nd[i - 1], this->pipes_2nd[i], &current_x, &current_y);
		}

		this->vibrator->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
		current_x += pipe_length + snip_width;
		current_y += pipe_thickness - snip_height;
		this->bench->move_to(this->vibrator, current_x, current_y);

		{ // flow water and oil pipes and scales
			Rect mport = this->master->get_input_port();
			Rect cport = this->cleaner->get_output_port();
			Rect pport = this->pipes_1st[0]->get_input_port();
			float pipe_ascent = pport.Y;
			float pipe_x, pipe_y, liquid_xoff, liquid_yoff;
			float scale_x, scale_y, scale_width, scale_height;

			this->bench->fill_snip_bound(this->pipes_1st[0], &pipe_x, &pipe_y, nullptr, nullptr);
			this->bench->fill_snip_bound(this->oil_pipes[1], nullptr, &liquid_yoff, nullptr, nullptr);
			this->Pps[0]->fill_extent(0.0F, 0.0F, &scale_width, &scale_height);
			this->water_pipes[0]->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

			current_y = pipe_y + pipe_ascent;
			scale_x = pipe_x + (pipe_length - scale_width) * 0.5F;
			scale_y = current_y + pport.Height;
			liquid_xoff = (pipe_length - snip_width) * 0.5F;
			liquid_yoff = pipe_ascent + liquid_yoff - snip_height;
			for (unsigned char i = 1; i < SNIPS_ARITY(this->oil_pipes); i++) {
				float sx = scale_x + pipe_length * (i - 1);

				this->bench->move_to(this->oil_pipes[i], pipe_x + liquid_xoff + pipe_length * (i - 1), pipe_y + liquid_yoff);
				this->bench->move_to(this->Pps[i - 1],  sx, current_y - scale_height);
				this->bench->move_to(this->Tpis[i - 1], sx, scale_y + scale_height * 0.0F);
				this->bench->move_to(this->Tpos[i - 1], sx, scale_y + scale_height * 1.0F);
			}

			liquid_yoff = liquid_yoff + pipe_thickness + snip_width - pipe_ascent * 2.0F;
			this->bench->fill_snip_bound(this->master, &pipe_x, nullptr, nullptr, nullptr);
			scale_x = pipe_x + mport.X + (mport.Width - scale_width) * 0.5F;
			current_x = pipe_x + mport.X + (mport.Width - snip_width) * 0.5F;
			current_y = pipe_y + (pipe_thickness - snip_height) * 0.5F;
			this->bench->move_to(this->oil_pipes[0], pipe_x + this->master->get_motor_port().X - snip_width, current_y);
			this->bench->move_to(this->Pss[B::Master], scale_x, scale_y);
			this->bench->move_to(this->Tss[B::Master], scale_x, scale_y + scale_height);
			
			this->bench->fill_snip_bound(this->cleaner, &pipe_x, &pipe_y, nullptr, nullptr);
			scale_x = pipe_x + (cport.X - scale_width) * 0.5F;
			this->bench->move_to(this->water_pipes[0], pipe_x + cport.X + cport.Width, current_y);
			this->bench->move_to(this->Pss[B::Cleaner], scale_x, scale_y);
			this->bench->move_to(this->Tss[B::Cleaner], scale_x, scale_y + scale_height);

			scale_x = pipe_x + cport.X + (cport.Width - scale_width) * 0.5F;
			scale_y = pipe_y + cport.Y + pport.Height;
			this->bench->move_to(this->Pss[B::Slave], scale_x, scale_y);
			this->bench->move_to(this->Tss[B::Slave], scale_x, scale_y + scale_height);

			this->bench->move_to(this->water_pipes[1], current_x, pipe_y + liquid_yoff);
			current_x = pipe_x + cport.X + (cport.Width - snip_width) * 0.5F;
			this->bench->fill_snip_bound(this->pipes_2nd[0], &pipe_x, &pipe_y, nullptr, nullptr);
			this->bench->move_to(this->water_pipes[2], current_x, pipe_y + liquid_yoff);
			for (unsigned char i = 3; i < SNIPS_ARITY(this->water_pipes); i++) {
				this->bench->move_to(this->water_pipes[i], pipe_x + liquid_xoff + pipe_length * (i - 3), pipe_y + liquid_yoff);
			}
		}
	}

private:
	void move_motor(B id, IMotorSnip* pipe, float x, float y, double fx = 1.0, double fy = 1.0) {
		connect_motor(this->bench, pipe, this->motors[id], this->Tms[id], x, y, fx, fy);
	}

// never deletes these snips mannually
private:
	Statuslet * statusbar;
	Snip* icons[1];
	Gaugelet* gauges[B::Count];

private:
	Screwlet * master;
	Screwlet* slave;
	GlueCleanerlet* cleaner;
	Scalelet* Pss[3];
	Scalelet* Tss[3];
	Funnellet* funnel;
	Vibratorlet* vibrator;
	Pipelet* pipes_1st[4];
	Scalelet* Pps[4];
	Scalelet* Tpis[4];
	Scalelet* Tpos[4];
	Pipelet* pipes_2nd[2];
	Motorlet* motors[B::Count];
	Scalelet* Tms[B::Count];
	Liquidlet* oil_pipes[5];
	Liquidlet* water_pipes[5];

private:
	Platform::String^ caption;
	BSegment* bench;

private:
	float console_y;
};

private class BConsoleDecorator : public IUniverseDecorator {
public:
	BConsoleDecorator(Platform::String^ caption, Color& caption_color, float fontsize) {
		auto font = make_text_format("Consolas", fontsize);

		this->color = make_solid_brush(caption_color);
		this->caption = make_text_layout(speak(caption), font);
	};

public:
	void draw_before(IUniverse* self, CanvasDrawingSession^ ds, float Width, float Height) {
		float x = (Width - this->caption->LayoutBounds.Width) * 0.5F;
		float y = this->caption->DrawBounds.Y - this->caption->LayoutBounds.Y;

		ds->DrawTextLayout(this->caption, x, y, this->color);
	}

	void draw_before_snip(Snip* self, CanvasDrawingSession^ ds, float x, float y, float width, float height) override {
		if ((x == 0.0) && (y == 0.0)) { // underline statusbar
			ds->DrawLine(0, height, width, height, this->color, 2.0F);
		}
	}

private:
	ICanvasBrush^ color;
	CanvasTextLayout^ caption;
};

BSegment::BSegment(Panel^ parent, Platform::String^ caption) : Universe(parent, 8) {
	this->console = new BConsole(this, caption);
	this->set_decorator(new BConsoleDecorator("B1", system_color(UIElementType::GrayText), 64.0F));
}

BSegment::~BSegment() {
	if (this->console != nullptr) {
		delete this->console;
	}
}

void BSegment::load(CanvasCreateResourcesEventArgs^ args, float width, float height) {
	auto console = dynamic_cast<BConsole*>(this->console);

	if (console != nullptr) {
		console->load_window_frame(width, height);
		console->load_icons(width, height);
		console->load_gauges(width, height);
		console->load_workline(width, height);
	}
}

void BSegment::reflow(float width, float height) {
	auto console = dynamic_cast<BConsole*>(this->console);

	if (console != nullptr) {
		console->reflow_window_frame(width, height);
		console->reflow_icons(width, height);
		console->reflow_gauges(width, height);
		console->reflow_workline(width, height);
	}
}
