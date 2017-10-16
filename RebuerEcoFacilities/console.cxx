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
#include "layout/orientation.hpp"
#include "layout/absolute.hpp"
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
    BigBang(Panel^ parent) : Universe(parent, 8) {
        this->set_decorator(new BorderDecorator(true, true));
    }

public:
    void load(CanvasCreateResourcesEventArgs^ args) {
        this->insert(new Statuslet(speak("RRB1")), 0.0F, 0.0F);
        this->insert(new StorageTanklet(80.0F, 128.0F), 0.0F, 32.0F);
        this->insert(new Funnellet(64.0F, 64.0F), 128.0F, 48.0F);
        this->insert(new Motorlet(16.0F), 400.0F, 300.0F, 90.0);
        this->insert(new Motorlet(32.0F), 148.0F, 96.0F, -45.0);
        this->insert(new Motorlet(64.0F), 256.0F, 48.0F, -90.0);
        this->insert(new Motorlet(128.0F), 256.0F, 128.0F);
        this->insert(new Vibratorlet(32.0F), 700.0F, 300.0F);
    };
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

/*
void Console::initialize_component(Size region) {
    this->gauge->insert(new Gaugelet(speak("mastermotor"),  100, 100));
    this->gauge->insert(new Gaugelet(speak("feedingmotor"), 200, 100));
    this->gauge->insert(new Gaugelet(speak("cleanmotor"),   10,  20));
    this->gauge->insert(new Gaugelet(speak("slavermotor"),  200, 100));
    this->taskbar->insert(new Textlet(ref new Platform::String(L"TaskBar")));
}
*/

void Console::reflow(float width, float height) {
    this->universe->resize(width, height);
}

void Console::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
