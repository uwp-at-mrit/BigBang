#include "console.hxx"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

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
        this->universe = new BSegment(this, "B1", "192.168.0.188");
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
