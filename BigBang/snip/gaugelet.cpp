#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "snip/gaugelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Gaugelet::Gaugelet(Platform::String^ caption, int maxA, int maxn, char step) {
    this->caption = caption;
    this->Ampere = maxA;
    this->RPM = maxn;
    this->step = step;

    this->label_font = make_text_format(12.0F);
    this->scale_font = make_text_format(8.00F);

    this->width = 0.0F;
}

void Gaugelet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    if (this->width == 0.0F) {
        TextExtent ts = get_text_extent(this->caption, this->label_font);
        TextExtent l00 = get_text_extent("100", this->scale_font);
        
        // TODO: find out a scalable formula to calculate the size for subcomponents. 
        this->caption_width = ts.width;
        this->scale_height = l00.height;
        this->label_height = ts.height;
        this->meter_gapsize = l00.width * 1.618F;
        this->meter_y = ts.height * 1.618F;
        this->mark_width = l00.width / 3.0F;
        this->mark_interval = l00.height * 0.7F;
        this->meter_width = this->mark_width * 9.0F;

        this->width = std::fmax(this->caption_width, this->meter_width * 2.0F + this->meter_gapsize);
        this->height = this->meter_y + this->mark_interval * this->step + this->label_height * 2.0F + this->scale_height;
    }

    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Gaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto meters_width = this->meter_width * 2.0F + this->meter_gapsize;
    auto xA = x + (this->width - meters_width) / 2.0F;
    auto xR = xA + this->meter_width + this->meter_gapsize;
    
    ds->DrawText(this->caption, x + (this->width - this->caption_width) / 2.0F, y, Colors::Khaki, this->label_font);
    this->draw_meter(ds, xA, this->meter_y, this->Ampere, this->ampere, speak("ampere"), Colors::RoyalBlue);
    this->draw_meter(ds, xR, this->meter_y, this->RPM, float(this->rpm), speak("rpm"), Colors::Green);
}

void Gaugelet::draw_meter(CanvasDrawingSession^ ds, float x, float y, int mscale, float scale, Platform::String^ label, Color& color) {
    auto body_yoff = this->scale_height * 0.5F;
    auto body_width = (this->meter_width - this->mark_width) / 2.0F;
    auto body_height = this->mark_interval * this->step;
    auto body_x = x + this->meter_width - body_width;
    auto body_y = y + body_yoff;
    auto body_bottom = body_y + body_height;
    auto delta = mscale / this->step;
    auto long_mark_x = body_x - this->mark_width;
    auto short_mark_x = body_x - this->mark_width * 0.618F;
    auto current_height = std::fmin(scale / mscale * body_height, body_height);

    ds->FillRectangle(body_x, body_y, body_width, body_height - current_height, Colors::Gray);
    ds->FillRectangle(body_x, body_bottom - current_height, body_width, current_height, color);
    ds->DrawRectangle(body_x, body_y, body_width, body_height, Colors::DarkGray);
    ds->DrawLine(body_x, body_y, body_x, body_bottom, Colors::GhostWhite);

    for (char i = 0; i <= this->step; i ++) {
        auto ythis = body_y + this->mark_interval * i;
        if (i % 2 == 0) {
            auto sthis = (mscale - delta * i).ToString();
            auto xoff = this->mark_width * (3 - sthis->Length());
            ds->DrawText(sthis, x + xoff, ythis - body_yoff, Colors::White, this->scale_font);
            ds->DrawLine(body_x, ythis, long_mark_x, ythis, Colors::WhiteSmoke);
        } else {
            ds->DrawLine(body_x, ythis, short_mark_x, ythis, Colors::WhiteSmoke);
        }
    }

    { // draw current value and label
        auto scale_layout = ref new CanvasTextLayout(ds, scale.ToString(), this->label_font, 0.0f, 0.0f);
        auto label_layout = ref new CanvasTextLayout(ds, label, this->label_font, 0.0f, 0.0f);

        auto scale_x = body_x - scale_layout->LayoutBounds.Width / 2.0F;
        auto scale_y = body_bottom + body_yoff;
        auto label_x = body_x - label_layout->LayoutBounds.Width / 2.0F;

        ds->DrawTextLayout(scale_layout, scale_x, scale_y, Colors::Yellow);
        ds->DrawTextLayout(label_layout, label_x, scale_y + this->label_height, Colors::DarkKhaki);
    }
}
