#include <cstdlib>
#include <algorithm>

#include "rsyslog.hpp"
#include "console.hxx"
#include "universe.hpp"
#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
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

inline void connect_pipes(Universe* universe, IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx = 0.5, double fy = 0.5) {
    pipe_connecting_position(prev, pipe, x, y, fx, fy);
    universe->move_to(pipe, (*x), (*y));
}

class BSegment : public WarGrey::SCADA::Universe {
public:
    BSegment(Panel^ parent, Platform::String^ caption) : Universe(parent, 8) {
        this->caption = caption;

        // this->set_decorator(new BorderDecorator(true, true, true));
        // this->set_decorator(new PipelineDecorator(true, true));
        this->set_decorator(new GridDecorator());
    }

public:
    void load(CanvasCreateResourcesEventArgs^ args, float width, float height) override {
        { // load window UI
            this->statusbar = new Statuslet(this->caption);
            this->insert(this->statusbar);
        }

        { // load icons
            this->icons[0] = new StorageTanklet(80.0F);
            
            for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->insert(this->icons[i]);
            }
        }

        { // load gauges
            this->gauges[0] = new Gaugelet("mastermotor", 100, 100);
            this->gauges[1] = new Gaugelet("feedingmotor", 200, 100);
            this->gauges[2] = new Gaugelet("cleanmotor", 10, 20);
            this->gauges[3] = new Gaugelet("slavemotor", 200, 100);

            for (size_t i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
                this->insert(this->gauges[i]);
            }
        }

        { // load B Segment
            float pipe_length = 128.0F;
            float pipe_thickness = 32.0F;

            this->master = new LScrewlet(pipe_length, 128.0F, pipe_thickness);
            this->slave = new RScrewlet(164.0F, 80.0F, pipe_thickness);
            this->cleaner = new GlueCleanerlet(80.0F, 128.0F, pipe_thickness);
            this->funnel = new Funnellet(32.0F, 0.0F, 120.0, 0.7, 0.3, 0.84);
            this->vibrator = new Vibratorlet(pipe_thickness * 1.618F);

            this->insert(this->master);
            this->insert(this->funnel);
            this->insert(this->cleaner);
            this->insert(this->slave);
            this->insert(this->vibrator);

            for (size_t i = 0; i < sizeof(this->pipes) / sizeof(Snip*); i++) {
                this->pipes[i] = new LPipelet(pipe_length, 0.0F, pipe_thickness);
                this->insert(this->pipes[i]);
            }

            for (size_t i = 0; i < sizeof(this->hfpipes) / sizeof(Snip*); i++) {
                this->hfpipes[i] = new RPipelet(pipe_length, 0.0F, pipe_thickness, 120.0, 0.607, 0.339, 0.839);
                this->insert(this->hfpipes[i]);
            }
        }

        { // load motors
            this->motors[0] = new Motorlet(100);
            this->motors[1] = new Motorlet(100);
            this->motors[2] = new Motorlet(100);
            this->motors[3] = new Motorlet(100);

            for (size_t i = 0; i < sizeof(this->motors) / sizeof(Motorlet*); i++) {
                this->insert(this->motors[i]);
            }
        }
    };

    void reflow(float width, float height) override {
        float snip_width, snip_height, console_y, fitting_height;

        this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        
        { // flow icons
            float icon_gapsize = 64.0F;
            float icon_hmax = 0.0F;
            float icon_x = 0.0F;
            float icon_y = console_y * 1.5F;

            for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &snip_height);
                icon_hmax = max(snip_height, icon_hmax);
            }

            for (size_t i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
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
            for (unsigned int i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
                this->move_to(this->gauges[i], gauge_x, gauge_y);
                this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_width);
                gauge_x += (snip_width + gauge_gapsize);
            }
        }

        { // flow B Segment
            size_t pcount = sizeof(this->pipes) / sizeof(Snip*);
            size_t hfpcount = sizeof(this->hfpipes) / sizeof(Snip*);
            
            this->funnel->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

            float current_x = width * 0.25F;
            float current_y = (height - snip_height) * 0.25F;
            this->move_to(this->funnel, current_x, current_y);

            connect_pipes(this, this->funnel, this->master, &current_x, &current_y, 0.25, 0.50);
            connect_pipes(this, this->master, this->pipes[0], &current_x, &current_y);

            for (size_t i = 1; i < pcount; i++) {
                connect_pipes(this, this->pipes[i - 1], this->pipes[i], &current_x, &current_y);
            }

            connect_pipes(this, this->pipes[pcount], this->cleaner, &current_x, &current_y);
            connect_pipes(this, this->cleaner, this->slave, &current_x, &current_y, 1.0);
            connect_pipes(this, this->slave, this->hfpipes[0], &current_x, &current_y);

            for (size_t i = 1; i < hfpcount; i++) {
                connect_pipes(this, this->hfpipes[i - 1], this->hfpipes[i], &current_x, &current_y);
            }

            this->pipes[0]->fill_extent(0.0F, 0.0F, nullptr, &fitting_height);
            this->vibrator->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
            this->move_to(this->vibrator, current_x - snip_width * 2.0F, current_y + (fitting_height - snip_height) * 0.5F);
        }
    }
    
// never deletes these snips mannually
private:
    Statuslet* statusbar;
    Snip* icons[1];
    Gaugelet* gauges[4];

private:
    Screwlet* master;
    Screwlet* slave;
    GlueCleanerlet* cleaner;
    Funnellet* funnel;
    Vibratorlet* vibrator;
    Pipelet* pipes[4];
    Motorlet* motors[4];
    Pipelet* hfpipes[2];

private:
    Platform::String^ caption;
};

Console::Console() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(4.0);
    //this->listener = ref new TCPListener((unsigned short)18030);
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
