#include <cstdlib>
#include <algorithm>

#include "workspace.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "snip/alarmlet.hpp"
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

WorkSpace::WorkSpace() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(0.0);

    //this->listener = ref new TCPListener((unsigned short)18030);
}

void WorkSpace::initialize_component(Size region) {
    this->stage = ref new Pasteboard(this, "stage", new AbsoluteLayout(400.0F, 300.0F));
    this->stage->inset = ThicknessHelper::FromUniformLength(0.0);

    this->reflow(region.Width, region.Height);
}

void WorkSpace::reflow(float width, float height) {
    this->stage->canvas_width = width;
    this->stage->canvas_height = height;
}

void WorkSpace::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
