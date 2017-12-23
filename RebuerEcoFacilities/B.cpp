#include <cstdlib>
#include <algorithm>

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

static inline Motorlet* construct_motorlet(IUniverse* universe, float width, double degree = 0.0) {
	Motorlet* motor = new Motorlet(width);
	universe->insert(motor, degree);

	return motor;
}

static inline Gaugelet* construct_gaugelet(IUniverse* universe, Platform::String^ caption, int A, int RPM) {
	Gaugelet* gauge = new Gaugelet(caption, A, RPM);
	universe->insert(gauge);

	return gauge;
}

static inline Liquidlet* construct_water_pipe(IUniverse* universe, float length, double degrees = 0.0) {
	Liquidlet* waterpipe = new Liquidlet(length, ArrowPosition::End, 209.60, 1.000, 0.559);
	universe->insert(waterpipe, degrees);

	return waterpipe;
}

static inline Liquidlet* construct_oil_pipe(IUniverse* universe, float length, double degrees = 0.0) {
	Liquidlet* oilpipe = new Liquidlet(length, ArrowPosition::Start, 38.825, 1.000, 0.500);
	universe->insert(oilpipe, degrees);

	return oilpipe;
}

static inline void connect_pipes(IUniverse* universe, IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx = 0.5, double fy = 0.5) {
    pipe_connecting_position(prev, pipe, x, y, fx, fy);
    universe->move_to(pipe, (*x), (*y));
}

static inline void place_motor(IUniverse* universe, IMotorSnip* pipe, Motorlet* motor, float x, float y, double fx = 1.0, double fy = 1.0) {
	// TODO: there must be a more elegant way to deal with rotated motors
	float motor_width, motor_height, yoff;
	Rect mport = pipe->get_motor_port();

	motor->fill_extent(0.0F, 0.0F, &motor_width, &motor_height);
	universe->fill_snip_bound(motor, nullptr, &yoff, nullptr, nullptr);

	x = x + mport.X + (mport.Width - motor_width) * float(fx);
	y = y + mport.Y + (mport.Height - motor_height + yoff) * float(fy);
	universe->move_to(motor, x, y);
}

class BConsole : public IUniverseDecorator {
public:
	BConsole(Platform::String^ caption, Color& caption_color, float fontsize) {
		this->color = make_solid_brush(caption_color);
		this->caption = make_text_layout(speak(caption), make_text_format(fontsize));
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
	this->caption = caption;
	this->set_decorator(new BConsole("B1", system_color(UIElementType::GrayText), 64.0F));
}

void BSegment::load(CanvasCreateResourcesEventArgs^ args, float width, float height) {
	{ // load window UI
		this->statusbar = new Statuslet(this->caption);
		this->insert(this->statusbar);
	}

	{ // load icons
		this->icons[0] = new StorageTanklet(80.0F);

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->insert(this->icons[i]);
		}
	}

	{ // load gauges
		this->gauges[BMotor::Master] = construct_gaugelet(this, "mastermotor", 100, 100);
		this->gauges[BMotor::Funnel] = construct_gaugelet(this, "feedingmotor", 200, 100);
		this->gauges[BMotor::Cleaner] = construct_gaugelet(this, "cleanmotor", 10, 20);
		this->gauges[BMotor::Slave] = construct_gaugelet(this, "slavemotor", 200, 100);
	}

	{ // load B Segment
		float pipe_length = 128.0F;
		float pipe_thickness = 32.0F;
		float master_height = 128.0F;
		float funnel_width = 42.0F;
		float slave_height = 80.0F;

		{ // load water and oil pipes
			this->water_pipes[0] = construct_water_pipe(this, pipe_length, 0);
			for (unsigned char i = 1; i < SNIPS_ARITY(this->water_pipes); i++) {
				this->water_pipes[i] = construct_water_pipe(this, pipe_length, 90.0);
			}

			this->oil_pipes[0] = construct_oil_pipe(this, pipe_length);
			for (unsigned char i = 1; i < SNIPS_ARITY(this->oil_pipes); i++) {
				this->oil_pipes[i] = construct_oil_pipe(this, pipe_length, 90.0);
			}
		}

		this->master = new LScrewlet(pipe_length, master_height, pipe_thickness);
		this->slave = new LScrewlet(pipe_length, slave_height, pipe_thickness);
		this->cleaner = new GlueCleanerlet(pipe_length, master_height, pipe_thickness);
		this->funnel = new Funnellet(funnel_width, 0.0F, 120.0, 0.7, 0.3, 0.84);
		this->vibrator = new Vibratorlet(pipe_thickness * 1.618F);

		this->insert(this->master);
		this->insert(this->funnel);

		for (size_t i = 0; i < SNIPS_ARITY(this->pipes_1st); i++) {
			this->pipes_1st[i] = new LPipelet(pipe_length, 0.0F, pipe_thickness);
			this->insert(this->pipes_1st[i]);
		}

		this->insert(this->cleaner);
		this->insert(this->slave);

		for (size_t i = 0; i < SNIPS_ARITY(this->pipes_2nd); i++) {
			this->pipes_2nd[i] = new LPipelet(pipe_length, 0.0F, pipe_thickness, 120.0, 0.607, 0.339, 0.839);
			this->insert(this->pipes_2nd[i]);
		}

		this->insert(this->vibrator);

		{ // load motors
			this->motors[BMotor::Funnel] = construct_motorlet(this, funnel_width, 90.0);
			this->motors[BMotor::Master] = construct_motorlet(this, master_height * 0.85F);
			this->motors[BMotor::Slave] = construct_motorlet(this, slave_height * 0.85F);
			this->motors[BMotor::Cleaner] = construct_motorlet(this, pipe_thickness, 90.0);
		}
	}
}

void BSegment::reflow(float width, float height) {
	float console_y, pipe_length, pipe_thickness, snip_width, snip_height;

	this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        
	{ // flow icons	
		float icon_gapsize = 64.0F;
		float icon_hmax = 0.0F;
		float icon_x = 0.0F;
		float icon_y = console_y * 1.5F;

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &snip_height);
			icon_hmax = max(snip_height, icon_hmax);
		}

		for (size_t i = 0; i < SNIPS_ARITY(this->icons) && this->icons[i] != nullptr; i++) {
			this->icons[i]->fill_extent(icon_x, icon_y, &snip_width, &snip_height);
			this->move_to(this->icons[i], icon_x, icon_y + (icon_hmax - snip_height) * 0.5F);
			icon_x += (snip_width + icon_gapsize);
		}
	}

	{ // flow gauges
		float gauge_gapsize = 32.0F;
		float gauge_x = 0.0F;
		float gauge_y = console_y;

		this->gauges[0]->fill_extent(gauge_x, gauge_y, nullptr, &snip_height);
		gauge_y = height - snip_height;
		for (unsigned int i = 0; i < SNIPS_ARITY(this->gauges); i++) {
			this->move_to(this->gauges[i], gauge_x, gauge_y);
			this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
			gauge_x += (snip_width + gauge_gapsize);
		}
	}

	{ // flow B Segment
		size_t pcount_1st = SNIPS_ARITY(this->pipes_1st);
		size_t pcount_2nd = SNIPS_ARITY(this->pipes_2nd);

		this->pipes_1st[0]->fill_extent(0.0F, 0.0F, &pipe_length, &pipe_thickness);
		this->funnel->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

		float current_x = pipe_length * 1.618F;
		float current_y = (height - snip_height) * 0.25F;
		this->move_to(this->funnel, current_x, current_y);
		place_motor(this, this->funnel, this->motors[BMotor::Funnel], current_x, current_y, 0.5, 1.0);

		connect_pipes(this, this->funnel, this->master, &current_x, &current_y, 0.2, 0.5);
		place_motor(this, this->master, this->motors[BMotor::Master], current_x, current_y);
		connect_pipes(this, this->master, this->pipes_1st[0], &current_x, &current_y);

		for (size_t i = 1; i < pcount_1st; i++) {
			connect_pipes(this, this->pipes_1st[i - 1], this->pipes_1st[i], &current_x, &current_y);
		}

		connect_pipes(this, this->pipes_1st[pcount_1st], this->cleaner, &current_x, &current_y);
		place_motor(this, this->cleaner, this->motors[BMotor::Cleaner], current_x, current_y, 0.5, 1.0);
		connect_pipes(this, this->cleaner, this->slave, &current_x, &current_y);
		place_motor(this, this->slave, this->motors[BMotor::Slave], current_x, current_y);
		connect_pipes(this, this->slave, this->pipes_2nd[0], &current_x, &current_y);

		for (size_t i = 1; i < pcount_2nd; i++) {
			connect_pipes(this, this->pipes_2nd[i - 1], this->pipes_2nd[i], &current_x, &current_y);
		}

		this->vibrator->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
		current_x += pipe_length + snip_width;
		current_y += pipe_thickness - snip_height;
		this->move_to(this->vibrator, current_x, current_y);

		{ // flow water and oil pipes
			Rect mport = this->master->get_input_port();
			Rect cport = this->cleaner->get_output_port();
			float pipe_ascent = this->pipes_1st[0]->get_input_port().Y;
			float pipe_x, pipe_y, liquid_xoff, liquid_yoff;

			this->fill_snip_bound(this->pipes_1st[0], &pipe_x, &pipe_y, nullptr, nullptr);
			this->fill_snip_bound(this->oil_pipes[1], nullptr, &liquid_yoff, nullptr, nullptr);
			this->water_pipes[0]->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

			liquid_xoff = (pipe_length - snip_width) * 0.5F;
			liquid_yoff = pipe_ascent + liquid_yoff - snip_height;
			for (unsigned char i = 1; i < SNIPS_ARITY(this->oil_pipes); i++) {
				this->move_to(this->oil_pipes[i], pipe_x + liquid_xoff + pipe_length * (i - 1), pipe_y + liquid_yoff);
			}

			liquid_yoff = liquid_yoff + pipe_thickness + snip_width - pipe_ascent * 2.0F;
			this->fill_snip_bound(this->master, &pipe_x, nullptr, nullptr, nullptr);
			current_x = pipe_x + mport.X + (mport.Width - snip_width) * 0.5F;
			current_y = pipe_y + (pipe_thickness - snip_height) * 0.5F;
			this->move_to(this->oil_pipes[0], pipe_x + this->master->get_motor_port().X - snip_width, current_y);
			this->fill_snip_bound(this->cleaner, &pipe_x, nullptr, nullptr, nullptr);
			this->move_to(this->water_pipes[0], pipe_x + cport.X + cport.Width, current_y);

			this->move_to(this->water_pipes[1], current_x, pipe_y + liquid_yoff);
			current_x = pipe_x + cport.X + (cport.Width - snip_width) * 0.5F;
			this->fill_snip_bound(this->pipes_2nd[0], &pipe_x, &pipe_y, nullptr, nullptr);
			this->move_to(this->water_pipes[2], current_x, pipe_y + liquid_yoff);
			for (unsigned char i = 3; i < SNIPS_ARITY(this->water_pipes); i++) {
				this->move_to(this->water_pipes[i], pipe_x + liquid_xoff + pipe_length * (i - 3), pipe_y + liquid_yoff);
			}
		}
	}
}
