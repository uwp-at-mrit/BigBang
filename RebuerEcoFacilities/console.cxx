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

class BigBang : public WarGrey::SCADA::Universe {
public:
    BigBang(Panel^ parent, Platform::String^ caption) : Universe(parent, 8) {
        this->caption = caption;

        this->set_decorator(new BorderDecorator(true, false, true));
    }

public:
    void load(CanvasCreateResourcesEventArgs^ args, float width, float height) override {
        this->statusbar = new Statuslet(this->caption);
        this->icons[0] = new StorageTanklet(80.0F);
        this->icons[1] = new Funnellet(64.0F, 0.0F, 32.0, 1.0, 0.4, 0.8);
        this->icons[2] = new Funnellet(32.0F, 0.0F, 120.0, 0.7, 0.3, 0.84);
        this->icons[3] = new HPipelet(108.0F, 32.0F, 120.0);
        this->motor = new Motorlet(169.0F);
        this->vibrator = new Vibratorlet(96.0F);

        this->insert(this->statusbar);
        this->insert(this->vibrator);
        this->insert(this->motor);

        this->gauges[0] = new Gaugelet("mastermotor", 100, 100);
        this->gauges[1] = new Gaugelet("feedingmotor", 200, 100);
        this->gauges[2] = new Gaugelet("cleanmotor", 10, 20);
        this->gauges[3] = new Gaugelet("slavemotor", 200, 100);

        for (unsigned int i = 0; i < sizeof(this->icons) / sizeof(Snip*); i++) {
            this->insert(this->icons[i]);
        }

        for (unsigned int i = 0; i < sizeof(this->gauges) / sizeof(Gaugelet*); i++) {
            this->insert(this->gauges[i]);
        }
    };

    void reflow(float width, float height) override {
        float snip_width, snip_height;
        float console_y;

        this->statusbar->fill_extent(0.0F, 0.0F, nullptr, &console_y);
        this->vibrator->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
        this->move_to(this->vibrator, (width - snip_width) * 0.5F, (height - snip_height) * 0.5F);
        this->motor->fill_extent(0.0F, 0.0F, &snip_width, &snip_height);
        this->move_to(this->motor, width * 0.2F, (height - snip_height) * 0.5F);

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
    }

private: // never deletes these snips mannually
    Statuslet* statusbar;
    Vibratorlet* vibrator;
    Motorlet* motor;
    Snip* icons[4];
    Gaugelet* gauges[4];

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
        this->universe = new BigBang(this, "RRB1");
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
