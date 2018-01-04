#include <cmath>

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "shape.hpp"
#include "snip/gaugelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

Gaugelet::Gaugelet(Platform::String^ caption, int maxA, int maxn, unsigned char step, Color& acolor, Color& rcolor)
	: Ampere(maxA), RPM(maxn), step(step) {
	this->label_font = make_text_format(12.0F);
	this->caption = make_text_layout(speak(caption), this->label_font);
	this->Alabel = make_text_layout(speak("ampere"), this->label_font);
	this->Rlabel = make_text_layout(speak("rpm"), this->label_font);

	this->Acolor = make_solid_brush(acolor);
	this->Rcolor = make_solid_brush(rcolor);

	this->set_ampere(0.0F, true);
	this->set_rpm(0, true);
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
        this->Ascales = make_vertical_layout(Ascales, font, linespacing, CanvasHorizontalAlignment::Right);
        this->Rscales = make_vertical_layout(Rscales, font, linespacing, CanvasHorizontalAlignment::Right);
    }
}

void Gaugelet::load() {	
	this->initialize_meters();

    this->meter_gapsize = this->Rscales->LayoutBounds.Height - this->Rscales->DrawBounds.Height;
    this->meter_width = this->mark_width * 9.0F /* must greater than 5.0F */;

    this->width = std::fmax(this->caption->LayoutBounds.Width, this->meter_width * 2.0F + this->meter_gapsize);
    this->height = this->caption->LayoutBounds.Height * 2.618F + this->Ascales->LayoutBounds.Height;
}

void Gaugelet::set_ampere(float a, bool force) {
	if ((this->ampere != a) || force) {
		this->ampere = a;
		this->Alayout = make_text_layout(a.ToString(), this->label_font);
	}
}

void Gaugelet::set_rpm(int rpm, bool force) {
	if ((this->rpm != rpm) || force) {
		this->rpm = float(rpm);
		this->Rlayout = make_text_layout(rpm.ToString(), this->label_font);
	}
}

void Gaugelet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Gaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float meters_width = this->meter_width * 2.0F + this->meter_gapsize;
    float xA = x + (this->width - meters_width) * 0.5F;
    float xR = xA + this->meter_width + this->meter_gapsize;
    float meter_y = y + this->caption->LayoutBounds.Height;
    
    ds->DrawTextLayout(this->caption, x + (this->width - this->caption->LayoutBounds.Width) * 0.5F, y, Colors::Khaki);
    this->draw_meter(ds, xA, meter_y, this->ampere, this->Ampere, this->Ascales, this->Alayout, this->Alabel, this->Acolor);
    this->draw_meter(ds, xR, meter_y, this->rpm, this->RPM, this->Rscales, this->Rlayout, this->Rlabel, this->Rcolor);
}

void Gaugelet::draw_meter(CanvasDrawingSession^ ds, float x, float y, float scale, int mscale
	, CanvasTextLayout^ scales_layout, CanvasTextLayout^ scale_layout, CanvasTextLayout^ label_layout
	, ICanvasBrush^ color) {
    float body_yoff = scales_layout->DrawBounds.Y + this->mark_interval * 0.5F;
    float body_x = x + this->mark_width * 5.0F;
    float body_y = y + body_yoff;
	float body_height = this->mark_interval * float(this->step);
    float body_width = this->meter_width + x - body_x;
	float current_height = std::fmin(scale * body_height / float(mscale), body_height);

    ds->FillRectangle(body_x, body_y, body_width, body_height, Colors::Gray);
    ds->FillRectangle(body_x, body_y + body_height - current_height, body_width, current_height, color);
    ds->DrawRectangle(body_x, body_y, body_width, body_height, Colors::GhostWhite);
    
    { // draw scales and marks
        float scale_xoff = -scales_layout->LayoutBounds.X - (this->mark_width * 2.0F);
        float scale_width = scales_layout->LayoutBounds.Width;
        float scale_spacing = scales_layout->DrawBounds.Y;

        // WARNING: the box mode of the text layout is not intuitive
        ds->DrawTextLayout(scales_layout, body_x + scale_xoff - scale_width, y, Colors::WhiteSmoke);
        ds->DrawCachedGeometry(this->scale_marks, body_x - this->mark_width, body_y, Colors::WhiteSmoke);
    }

    { // draw current value and label
		float label_height = this->caption->LayoutBounds.Height;
		float label_x = body_x - label_layout->LayoutBounds.Width * 0.5F;
		float scale_x = body_x - scale_layout->LayoutBounds.Width * 0.5F;
		float scale_y = y + this->height - (label_height * 3.0F);

		ds->DrawTextLayout(scale_layout, scale_x, scale_y, Colors::Yellow);
		ds->DrawTextLayout(label_layout, label_x, scale_y + label_height, Colors::DarkKhaki);
    }
}
