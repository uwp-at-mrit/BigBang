#include <cstdlib>
#include <algorithm>

#include "tongue.hpp"
#include "workspace.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "layout/orientation.hpp"
#include "layout/absolute.hpp"

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

WorkSpace::WorkSpace() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = zero;

    //this->listener = ref new TCPListener((unsigned short)18030);
}

void WorkSpace::initialize_component(Size region) {
    this->statusbar = ref new Pasteboard(this, new HorizontalLayout(0.0F));
    this->statusbar->show_border(false);
    this->statusbar->show_selection_dots(false);
    this->statusbar->insert(new Statuslet(speak("RRB1")), 0.0F, 0.0F);
    
    this->stage = ref new Pasteboard(this, new AbsoluteLayout());
    this->stage->show_selection_dots(false);
    //this->stage->show_border(false);
    //this->stage->inset = zero;

    this->taskbar = ref new Pasteboard(this, new HorizontalLayout(0.0F));
    this->taskbar->show_border(false);
    this->taskbar->show_inset_box(true);
    this->taskbar->show_selection_dots(false);
    this->taskbar->insert(new Textlet(ref new Platform::String(L"TaskBar")), 0.0F, 0.0F);

    this->reflow(region.Width, region.Height);
}

void WorkSpace::reflow(float width, float height) {
    float sbar_height = this->statusbar->actual_height;
    float tbar_height = this->taskbar->actual_height;

    this->statusbar->canvas_width = width;
    this->taskbar->canvas_width = width;
    this->stage->canvas_width = width;

    if (sbar_height == 0.0F) {
        this->statusbar->fill_snips_bounds(nullptr, nullptr, nullptr, &sbar_height);
        sbar_height += float(this->statusbar->inset.Top + this->statusbar->inset.Bottom);
    }

    if (tbar_height == 0.0F) {
        this->taskbar->fill_snips_bounds(nullptr, nullptr, nullptr, &tbar_height);
        tbar_height += float(this->taskbar->inset.Top + this->taskbar->inset.Bottom);
    }

    this->stage->canvas_height = height - sbar_height - tbar_height;
}

void WorkSpace::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
