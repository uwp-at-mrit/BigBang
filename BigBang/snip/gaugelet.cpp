#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "shape.hpp"
#include "snip/gaugelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Gaugelet::Gaugelet(Platform::String^ caption, int maxA, int maxn, unsigned char step, Color acolor, Color rcolor)
	: Ampere(maxA), RPM(maxn), step(step), Acolor(acolor), Rcolor(rcolor) {
	this->caption = speak(caption);
	this->label_font = make_text_format(12.0F);
}

void Gaugelet::initialize_meters() {
    auto font = make_text_format(8.00F);
    TextExtent l00 = get_text_extent("100", font);

    this->mark_width = l00.width / 3.0F;
    this->mark_interval = l00.height * 0.7F;

    { // make scales and marks
        float linespacing = this->mark_interval * 2.0F;
        float short_mark_length = this->mark_width * 0.618F;
        float short_mark_x = this->mark_width - short_mark_length;
        int Adelta = this->Ampere / this->step;
        int Rdelta = this->RPM / this->step;

        auto marks = blank();
        Platform::String^ Ascales = "";
        Platform::String^ Rscales = "";
        for (char i = 0; i <= step; i++) {
            auto ythis = mark_interval * i;

            if (i % 2 == 0) {
                marks = geometry_union(marks, hline(0.0F, ythis, mark_width));
                Ascales = Ascales + " " + (this->Ampere - Adelta * i).ToString();
                Rscales = Rscales + " " + (this->RPM - Rdelta * i).ToString();
            } else {
                marks = geometry_union(marks, hline(short_mark_x, ythis, short_mark_length));
            }
        }

        this->scale_marks = geometry_freeze(marks);
        this->ampere_scales = make_vertical_layout(Ascales, font, linespacing, CanvasHorizontalAlignment::Right);
        this->rpm_scales = make_vertical_layout(Rscales, font, linespacing, CanvasHorizontalAlignment::Right);
    }
}

void Gaugelet::load() {
    TextExtent ts = get_text_extent(this->caption, this->label_font);

    this->initialize_meters();

    this->caption_width = ts.width;
    this->label_height = ts.height;
    this->meter_gapsize = this->rpm_scales->LayoutBounds.Height - this->rpm_scales->DrawBounds.Height;
    this->meter_width = this->mark_width * 9.0F /* must greater than 5.0F */;

    this->width = std::fmax(this->caption_width, this->meter_width * 2.0F + this->meter_gapsize);
    this->height = this->label_height * 2.618F + this->ampere_scales->LayoutBounds.Height;
}

void Gaugelet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Gaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float meters_width = this->meter_width * 2.0F + this->meter_gapsize;
    float xA = x + (this->width - meters_width) * 0.5F;
    float xR = xA + this->meter_width + this->meter_gapsize;
    float meter_y = y + this->label_height;
    
    ds->DrawText(this->caption, x + (this->width - this->caption_width) * 0.5F, y, Colors::Khaki, this->label_font);
    this->draw_meter(ds, xA, meter_y, this->ampere_scales, this->Ampere, this->ampere, "ampere", this->Acolor);
    this->draw_meter(ds, xR, meter_y, this->rpm_scales, this->RPM, float(this->rpm), "rpm", this->Rcolor);
}

void Gaugelet::draw_meter(CanvasDrawingSession^ ds, float x, float y
    , CanvasTextLayout^ scales, int mscale, float scale, Platform::String^ label
    , Color& color) {
    float body_yoff = scales->DrawBounds.Y + this->mark_interval * 0.5F;
    float body_x = x + this->mark_width * 5.0F;
    float body_y = y + body_yoff;
    float body_width = this->meter_width + x - body_x;
    float body_height = this->mark_interval * float(this->step);
    float current_height = std::fmin(scale * body_height / float(mscale), body_height);

    ds->FillRectangle(body_x, body_y, body_width, body_height, Colors::Gray);
    ds->FillRectangle(body_x, body_y + body_height - current_height, body_width, current_height, color);
    ds->DrawRectangle(body_x, body_y, body_width, body_height, Colors::GhostWhite);
    
    { // draw scales and marks
        float scale_xoff = -scales->LayoutBounds.X - (this->mark_width * 2.0F);
        float scale_width = scales->LayoutBounds.Width;
        float scale_spacing = scales->DrawBounds.Y;

        // WARNING: the box mode of the text layout is not intuitive
        ds->DrawTextLayout(scales, body_x + scale_xoff - scale_width, y, Colors::WhiteSmoke);
        ds->DrawCachedGeometry(this->scale_marks, body_x - this->mark_width, body_y, Colors::WhiteSmoke);
    }

    { // draw current value and label
        auto scale_layout = make_text_layout(scale.ToString(), this->label_font);
        auto label_layout = make_text_layout(speak(label), this->label_font);

        float label_x = body_x - label_layout->LayoutBounds.Width * 0.5F;
        float scale_x = body_x - scale_layout->LayoutBounds.Width * 0.5F;
        float scale_y = y + this->height - (this->label_height * 3.0F);

        ds->DrawTextLayout(scale_layout, scale_x, scale_y, Colors::Yellow);
        ds->DrawTextLayout(label_layout, label_x, scale_y + this->label_height, Colors::DarkKhaki);
    }
}
