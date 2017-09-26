#include <cstdlib>
#include <algorithm>

#include "tongue.hpp"
#include "workspace.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/funnellet.hpp"
#include "snip/gaugelet.hpp"
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

static Thickness zero(0.0, 0.0, 0.0, 0.0);

static Pasteboard^ make_region(Panel^ parent, IPasteboardLayout* layout, IPasteboardDecorator* border = nullptr) {
    auto region = ref new Pasteboard(parent, layout);

    region->show_selection_dots(false);

    if (border != nullptr) {
        region->set_decorator(border);
    }

    return region;
}

static float region_height(Pasteboard^ pb) {
    float height = pb->actual_height;

    if (height == 0.0F) {
        pb->fill_snips_bounds(nullptr, nullptr, nullptr, &height);
        height += float(pb->inset.Top + pb->inset.Bottom);
    }

    return height;
}

WorkSpace::WorkSpace() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = zero;

    //this->listener = ref new TCPListener((unsigned short)18030);
}

void WorkSpace::initialize_component(Size region) {
    this->statusbar = make_region(this, new HorizontalLayout(0.0F), new HBorderDecorator(false, true));
    this->stage = make_region(this, new AbsoluteLayout());
    this->gauge = make_region(this, new HorizontalLayout(16.0F));
    this->taskbar = make_region(this, new HorizontalLayout(0.0F), new HBorderDecorator(true, false));
    
    this->taskbar->show_selection_dots(false);

    this->statusbar->insert(new Statuslet(speak("RRB1")));
    this->stage->insert(new StorageTanklet(80.0F, 128.0F));
    this->stage->insert(new Funnellet(80.0F, 64.0F), 128.0F, 64.0F);
    this->gauge->insert(new Gaugelet(speak("mastermotor"),  100, 100));
    this->gauge->insert(new Gaugelet(speak("feedingmotor"), 200, 100));
    this->gauge->insert(new Gaugelet(speak("cleanmotor"),   10,  20));
    this->gauge->insert(new Gaugelet(speak("slavermotor"),  200, 100));
    this->taskbar->insert(new Textlet(ref new Platform::String(L"TaskBar")));

    this->reflow(region.Width, region.Height);
}

void WorkSpace::reflow(float width, float height) {
    this->statusbar->canvas_width = width;
    this->taskbar->canvas_width = width;
    this->stage->canvas_width = width;
    this->gauge->canvas_width = width;

    this->stage->canvas_height = height
        - region_height(this->statusbar)
        - region_height(this->taskbar)
        - region_height(this->gauge);
}

void WorkSpace::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
