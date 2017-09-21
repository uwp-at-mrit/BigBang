#include <cmath>

#include "ui.hpp"
#include "time.hpp"
#include "pasteboard.hxx"
#include "snip/statuslet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

ref class WarGrey::SCADA::StatusTimer sealed { // delegate only accepts C++/CX class
public:
    void do_update_on_tick(Platform::Object^ sender, Platform::Object^ e) {
        this->timer->Interval = master->update_timestamp();
    }

internal:
    StatusTimer(Statuslet* master) {
        this->master = master;
        this->timer = gui_timer(1000, ref new ObjectHandler(this, &StatusTimer::do_update_on_tick));
    }

private:
    Windows::UI::Xaml::DispatcherTimer^ timer;
    Statuslet* master; // this will be destructed by Pasteboard.
};

/*************************************************************************************************/
Statuslet::Statuslet(Platform::String^ caption) {
    this->caption = this->caption;
    this->layout_config = ref new CanvasTextFormat();

    this->layout_config->WordWrapping = CanvasWordWrapping::NoWrap;
    this->layout_config->FontSize = 12;

    this->timer = ref new StatusTimer(this);
    this->update_timestamp();
}

TimeSpan Statuslet::update_timestamp() {
    int l00nanosecond;

    this->timestamp = update_nowstamp(&l00nanosecond);
    return TimeSpan{ 10000000 - l00nanosecond };
}

void Statuslet::fill_extent(float* width, float* height, float* bspace, float* tspace, float* lspace, float* rspace) {
    TextExtent ts = get_text_extent(this->timestamp, layout_config);

    if (width != nullptr) (*width) = ts.width;
    if (height != nullptr) (*height) = ts.height;
    if (bspace != nullptr) (*bspace) = ts.bspace;
    if (tspace != nullptr) (*tspace) = ts.tspace;
    if (lspace != nullptr) (*lspace) = ts.lspace;
    if (rspace != nullptr) (*rspace) = ts.rspace;
}

void Statuslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto layout = ref new CanvasTextLayout(ds, timestamp, layout_config, 0.0f, 0.0f);
    
    float tx = Width - layout->LayoutBounds.Width;

    ds->DrawText(this->caption, x, y, Colors::White, layout_config);
    ds->DrawTextLayout(layout, tx, y, Colors::White);
}
