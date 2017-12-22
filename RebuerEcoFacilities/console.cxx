#include <cstdlib>
#include <algorithm>

#include "console.hxx"
#include "universe.hpp"
#include "rsyslog.hpp"

#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
#include "snip/liquidlet.hpp"
#include "snip/pipeline/funnellet.hpp"
#include "snip/pipeline/pipelet.hpp"
#include "snip/pipeline/screwlet.hpp"
#include "snip/pipeline/gluecleanerlet.hpp"

#include "decorator/border.hpp"
#include "decorator/grid.hpp"
#include "decorator/pipeline.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::AppService;

using namespace Windows::UI;
using namespace Windows::UI::Core;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas::UI;

enum BMotor { Funnel = 0, Master, Cleaner, Slave, Count };

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

static inline void connect_motor(IUniverse* universe, IMotorSnip* pipe, Motorlet* motor, float x, float y, double fx = 1.0, double fy = 1.0) {
	// TODO: there must be a more elegant way to deal with rotated motors
	float motor_width, motor_height, yoff;
	Rect mport = pipe->get_motor_port();

	motor->fill_extent(0.0F, 0.0F, &motor_width, &motor_height);
	universe->fill_snip_bound(motor, nullptr, &yoff, nullptr, nullptr);

	x = x + mport.X + (mport.Width - motor_width) * float(fx);
	y = y + mport.Y + (mport.Height - motor_height + yoff) * float(fy);
	universe->move_to(motor, x, y);
}

class BSegment : public WarGrey::SCADA::Universe {
public:
	BSegment(Panel^ parent, Platform::String^ caption) : Universe(parent, 8) {
		this->caption = caption;

        this->set_decorator(new BorderDecorator(true, true, true));
        // this->set_decorator(new PipelineDecorator(true, true, true));
		// this->set_decorator(new GridDecorator());
	}

public:
    void load(CanvasCreateResourcesEventArgs^ args, float width, float height) override {
        { // load window UI
            this->statusbar = new Statuslet(this->caption);
            this->insert(this->statusbar);
        }

        { // load icons
			// this->icons[0] = new StorageTanklet(80.0F);

			this->icons[0] = new Liquidlet(128.0F);

			for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
				if (this->icons[i] != nullptr) {
					this->insert(this->icons[i]);
				}
			}
		}

        { // load gauges
            this->gauges[BMotor::Master]  = construct_gaugelet(this, "mastermotor",  100, 100);
            this->gauges[BMotor::Funnel]  = construct_gaugelet(this, "feedingmotor", 200, 100);
            this->gauges[BMotor::Cleaner] = construct_gaugelet(this, "cleanmotor",   10,  20);
            this->gauges[BMotor::Slave]   = construct_gaugelet(this, "slavemotor",   200, 100);
        }

        { // load B Segment
            float pipe_length = 128.0F;
            float pipe_thickness = 32.0F;
			float master_height = 128.0F;
			float funnel_width = 42.0F;
			float slave_height = 80.0F;

			{ // load water and oil pipes
				//this->motors[BMotor::Funnel] = construct_motorlet(this, funnel_width, 90.0);
				//this->motors[BMotor::Master] = construct_motorlet(this, master_height * 0.85F);
				//this->motors[BMotor::Slave] = construct_motorlet(this, slave_height * 0.85F);
				//this->motors[BMotor::Cleaner] = construct_motorlet(this, pipe_thickness, 90.0);
			}

            this->master   = new LScrewlet(pipe_length, master_height, pipe_thickness);
            this->slave    = new LScrewlet(pipe_length, slave_height, pipe_thickness);
            this->cleaner  = new GlueCleanerlet(pipe_length, master_height, pipe_thickness);
            this->funnel   = new Funnellet(funnel_width, 0.0F, 120.0, 0.7, 0.3, 0.84);
            this->vibrator = new Vibratorlet(pipe_thickness * 1.618F);

            this->insert(this->master);
            this->insert(this->funnel);

            for (size_t i = 0; i < sizeof(this->pipes_1st) / sizeof(Snip*); i++) {
                this->pipes_1st[i] = new LPipelet(pipe_length, 0.0F, pipe_thickness);
                this->insert(this->pipes_1st[i]);
            }

            this->insert(this->cleaner);
            this->insert(this->slave);

            for (size_t i = 0; i < sizeof(this->pipes_2nd) / sizeof(Snip*); i++) {
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
    };

    void reflow(float width, float height) override {
        float console_y, pipe_width, pipe_height, snip_width, snip_height;

        this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        
        { // flow icons
            float icon_gapsize = 64.0F;
            float icon_hmax = 0.0F;
            float icon_x = 0.0F;
            float icon_y = console_y * 1.5F;

            for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
				if (this->icons[i] != nullptr) {
					this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &snip_height);
					icon_hmax = max(snip_height, icon_hmax);
				}
            }

            for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
				if (this->icons[i] != nullptr) {
					this->icons[i]->fill_extent(icon_x, icon_y, &snip_width, &snip_height);
					this->move_to(this->icons[i], icon_x, icon_y + (icon_hmax - snip_height) * 0.5F);
					icon_x += (snip_width + icon_gapsize);
				}
            }
        }

        { // flow gauges
            float gauge_gapsize = 32.0F;
            float gauge_x = 0.0F;
            float gauge_y = console_y;

            this->gauges[0]->fill_extent(gauge_x, gauge_y, nullptr, &snip_height);
            gauge_y = height - snip_height;
            for (unsigned int i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
                this->move_to(this->gauges[i], gauge_x, gauge_y);
                this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
                gauge_x += (snip_width + gauge_gapsize);
            }
        }

        { // flow B Segment
            size_t pcount_1st = sizeof(this->pipes_1st) / sizeof(Snip*);
            size_t pcount_2nd = sizeof(this->pipes_2nd) / sizeof(Snip*);
            
            this->pipes_1st[0]->fill_extent(0.0F, 0.0F, &pipe_width, &pipe_height);
            this->funnel->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

            float current_x = pipe_width * 1.618F;
            float current_y = (height - snip_height) * 0.25F;
            this->move_to(this->funnel, current_x, current_y);
			connect_motor(this, this->funnel, this->motors[BMotor::Funnel], current_x, current_y, 0.5, 1.0);

            connect_pipes(this, this->funnel, this->master, &current_x, &current_y);
			connect_motor(this, this->master, this->motors[BMotor::Master], current_x, current_y);
            connect_pipes(this, this->master, this->pipes_1st[0], &current_x, &current_y);

            for (size_t i = 1; i < pcount_1st; i++) {
                connect_pipes(this, this->pipes_1st[i - 1], this->pipes_1st[i], &current_x, &current_y);
            }

            connect_pipes(this, this->pipes_1st[pcount_1st], this->cleaner, &current_x, &current_y);
			connect_motor(this, this->cleaner, this->motors[BMotor::Cleaner], current_x, current_y, 0.5, 1.0);
            connect_pipes(this, this->cleaner, this->slave, &current_x, &current_y);
			connect_motor(this, this->slave, this->motors[BMotor::Slave], current_x, current_y);
            connect_pipes(this, this->slave, this->pipes_2nd[0], &current_x, &current_y);

            for (size_t i = 1; i < pcount_2nd; i++) {
                connect_pipes(this, this->pipes_2nd[i - 1], this->pipes_2nd[i], &current_x, &current_y);
            }

            this->vibrator->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
            current_x += pipe_width + snip_width;
            current_y += pipe_height - snip_height;
            this->move_to(this->vibrator, current_x, current_y);
        }
    }
    
// never deletes these snips mannually
private:
    Statuslet* statusbar;
    Snip* icons[1];
    Gaugelet* gauges[BMotor::Count];

private:
    Screwlet* master;
    Screwlet* slave;
    GlueCleanerlet* cleaner;
    Funnellet* funnel;
    Vibratorlet* vibrator;
    Pipelet* pipes_1st[4];
    Pipelet* pipes_2nd[2];
	Motorlet* motors[BMotor::Count];
	Liquidlet* oil_pipes[5];
	Liquidlet* water_pipes[5];

private:
    Platform::String^ caption;
};

Console::Console() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(4.0);
}

Console::~Console() {
    if (this->universe != nullptr) {
        delete this->universe;
    }
}

void Console::initialize_component(Size region) {
    if (this->universe == nullptr) {
        this->universe = new BSegment(this, "RRB1");
    }

    this->reflow(region.Width, region.Height);
}

void Console::reflow(float width, float height) {
    this->universe->resize(width, height);
}

void Console::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
