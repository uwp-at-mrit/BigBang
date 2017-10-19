#include <cstdlib>
#include <algorithm>

#include "rsyslog.hpp"
#include "tongue.hpp"
#include "console.hxx"
#include "universe.hpp"
#include "snip/textlet.hpp"
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

class BigBang : public WarGrey::SCADA::Universe {
public:
    BigBang(Panel^ parent) : Universe(parent, 8) {}

public:
    void load(CanvasCreateResourcesEventArgs^ args, float width, float height) override {
        this->statusbar = new Statuslet(speak("RRB1"));
        this->icons[0] = new StorageTanklet(80.0F, 128.0F);
        this->icons[1] = new Funnellet(64.0F, 64.0F, 36.0);
        this->icons[2] = new Funnellet(32.0F, 32.0F, 120.0, 0.7);
        this->funnel_motor = new Motorlet(32.0F);
        this->vibrator = new Vibratorlet(32.0F);

        this->insert(this->statusbar);
        this->insert(this->vibrator);

        this->gauges[0] = new Gaugelet(speak("mastermotor"), 100, 100);
        this->gauges[1] = new Gaugelet(speak("feedingmotor"), 200, 100);
        this->gauges[2] = new Gaugelet(speak("cleanmotor"), 10, 20);
        this->gauges[3] = new Gaugelet(speak("slavemotor"), 200, 100);

        for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
            this->insert(this->icons[i]);
        }

        this->insert(this->funnel_motor, -45.0);

        for (unsigned int i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
            this->insert(this->gauges[i]);
        }
    };

    void reflow(float width, float height) override {
        float snip_wdith, snip_height;
        float console_y;

        this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        this->vibrator->fill_extent(0.0F, 0.0F, &snip_wdith, &snip_height);
        this->move_to(this->vibrator, (width - snip_wdith) / 2.0F, (height - snip_height) / 2.0F);

        { // flow icons
            float icon_gapsize = 64.0F;
            float icon_hmax = 0.0F;
            float icon_x = 0.0F;
            float icon_y = console_y * 1.5F;
            float snip_x, snip_y;

            for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->icons[i]->fill_extent(icon_x, icon_y, nullptr, &snip_height);
                icon_hmax = std::max(snip_height, icon_hmax);
            }

            for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
                this->icons[i]->fill_extent(icon_x, icon_y, &snip_wdith, &snip_height);
                this->move_to(this->icons[i], icon_x, icon_y + (icon_hmax - snip_height) / 2.0F);
                icon_x += (snip_wdith + icon_gapsize);
            }

            { // flow associated motors
                this->fill_snip_location(this->icons[1], &snip_x, &snip_y, SnipCenterPoint::CB);
                this->move_to(this->funnel_motor, snip_x, snip_y, SnipCenterPoint::CC);
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
                this->gauges[i]->fill_extent(gauge_x, gauge_y, &snip_wdith);
                gauge_x += (snip_wdith + gauge_gapsize);
            }
        }
    }

private: // never deletes these snips mannually
    Statuslet* statusbar;
    Vibratorlet* vibrator;
    Motorlet* funnel_motor;
    Snip* icons[3];
    Gaugelet* gauges[4];
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
        this->universe = new BigBang(this);
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
