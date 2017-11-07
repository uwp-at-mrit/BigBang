#include <cstdlib>
#include <algorithm>

#include "rsyslog.hpp"
#include "console.hxx"
#include "universe.hpp"
#include "snip/pipelet.hpp"
#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/funnellet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
#include "decorator/border.hpp"

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

class BSegment : public WarGrey::SCADA::Universe {
public:
    BSegment(Panel^ parent, Platform::String^ caption) : Universe(parent, 8) {
        this->caption = caption;

        //this->set_decorator(new BorderDecorator(true, false, true));
    }

public:
    void load(CanvasCreateResourcesEventArgs^ args, float width, float height) override {
        { // load window UI
            this->statusbar = new Statuslet(this->caption);
            this->insert(this->statusbar);
        }

        { // load icons
            this->icons[0] = new StorageTanklet(80.0F);
            this->icons[1] = new Motorlet(200.0F);
            this->icons[2] = new Vibratorlet(80.0F);

            for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->insert(this->icons[i]);
            }
        }

        { // load gauges
            this->gauges[0] = new Gaugelet("mastermotor", 100, 100);
            this->gauges[1] = new Gaugelet("feedingmotor", 200, 100);
            this->gauges[2] = new Gaugelet("cleanmotor", 10, 20);
            this->gauges[3] = new Gaugelet("slavemotor", 200, 100);

            for (unsigned int i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
                this->insert(this->gauges[i]);
            }
        }

        { // load B Segment
            float pipe_thickness = 32.0F;

            this->master = new Screwlet(128.0F, 128.0F, pipe_thickness);
            this->cleaner = new GlueCleanerlet(80.0F, 100.0F, pipe_thickness);
            this->funnel = new Funnellet(48.0F, 0.0F, 120.0, 0.7, 0.3, 0.84);

            this->insert(this->master);

            for (unsigned int i = 0; i < sizeof(this->pipes) / sizeof(Snip*); i++) {
                this->pipes[i] = new Pipelet(128.0F, 0.0F, pipe_thickness);
                this->insert(this->pipes[i]);
            }

            this->insert(this->funnel);
            this->insert(this->cleaner);
        }
    };

    void reflow(float width, float height) override {
        float snip_width, snip_height, console_y;

        this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        
        { // flow icons
            float icon_gapsize = 64.0F;
            float icon_hmax = 0.0F;
            float icon_x = 0.0F;
            float icon_y = console_y * 1.5F;

            for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &snip_height);
                icon_hmax = max(snip_height, icon_hmax);
            }

            for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
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
            float in_x, in_y, in_width, in_height;
            float out_x, out_y, out_width, out_height;
            float current_x, current_y;
            
            this->master->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);

            float master_x = width * 0.2F;
            float master_y = (height - snip_height) / 3.0F;
            this->move_to(this->master, master_x, master_y);

            this->funnel->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
            this->master->fill_inport_extent(&in_x, &in_y, &in_width, &in_height);
            current_x = master_x + in_x + (in_width - snip_width) * 0.5F;
            current_y = master_y + in_y + in_height - snip_height;
            this->move_to(this->funnel, current_x, current_y);

            current_x = master_x;
            current_y = master_y;
            this->master->fill_outport_extent(&out_x, &out_y, &out_width, &out_height);
            for (unsigned int i = 0; i < sizeof(this->pipes) / sizeof(Snip*); i++) {
                Pipelet* pipe = pipes[i];

                pipe->fill_inport_extent(&in_x, &in_y, &in_width, &in_height);

                current_x += ((out_x - in_x) - (out_width - in_width));
                current_y += ((out_y - in_y) + (out_height - in_height) * 0.5F);
                this->move_to(pipe, current_x, current_y);
                
                pipe->fill_outport_extent(&out_x, &out_y, &out_width, &out_height);
            }

            this->cleaner->fill_inport_extent(&in_x, &in_y, &in_width, &in_height);
            float cleaner_x = current_x + (out_x - in_x) - (out_width - in_width);
            float cleaner_y = current_y + (out_y - in_y) + (out_height - in_height) * 0.5F;
            this->move_to(this->cleaner, cleaner_x, cleaner_y);
        }
    }
    
// never deletes these snips mannually
private:
    Statuslet* statusbar;
    Snip* icons[3];
    Gaugelet* gauges[4];

private:
    Screwlet* master;
    Screwlet* slave;
    Pipelet* pipes[4];
    GlueCleanerlet* cleaner;
    Funnellet* funnel;
    Vibratorlet* vibrator;

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
