#include "text.hpp"
#include "paint.hpp"
#include "colorspace.hpp"
#include "snip/arrowlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

DoubleArrowlet::DoubleArrowlet(float length, ArrowPosition position, double color, double saturation, double lightness) {
	this->length = length;
	this->position = position;
	this->arrowhead_size = 4.0F;
	this->arrow_brush = make_solid_brush(hsla(color, saturation, lightness, 1.00));
	this->pipe_brush = make_solid_brush(hsla(color, saturation, lightness, 0.40));
	this->scale_font = make_text_format(8.0F);

	{
		auto ts = get_text_extent("100.000", this->scale_font);
		this->scale_width = ts.width;
		this->scale_height = ts.height;
	}
}

void DoubleArrowlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->length, h, this->scale_height * 2.0F + this->arrowhead_size * 3.0F);
};

void DoubleArrowlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	CanvasTextLayout^ in_layout  = make_text_layout(this->in_temperature.ToString(),  this->scale_font);
	CanvasTextLayout^ out_layout = make_text_layout(this->out_temperature.ToString(), this->scale_font);
	float in_layout_xoff = this->arrowhead_size + (this->scale_width - in_layout->DrawBounds.Width) * 0.5F;
	float out_layout_xoff = this->arrowhead_size + (this->scale_width - out_layout->DrawBounds.Width) * 0.5F;

	float arrowsize = this->scale_width + this->arrowhead_size * 2.5F;
	float wingsize = this->arrowhead_size * 0.5F;
	float out_y = y + this->scale_height + wingsize;
	float in_y = out_y + wingsize + this->arrowhead_size + wingsize;
	float start_x = x;
	float end_x = x + arrowsize;

	if (this->position == ArrowPosition::End) {
		start_x = x + this->length - arrowsize;
		end_x = x + this->length;
	}

	ds->FillRectangle(x, in_y - wingsize, this->length, this->arrowhead_size, this->pipe_brush);
	ds->DrawLine(end_x, in_y, start_x,                      in_y,            this->arrow_brush);
	ds->DrawLine(end_x, in_y, end_x - this->arrowhead_size, in_y - wingsize, this->arrow_brush);
	ds->DrawLine(end_x, in_y, end_x - this->arrowhead_size, in_y + wingsize, this->arrow_brush);
	ds->DrawTextLayout(in_layout, start_x + in_layout_xoff, in_y + wingsize, Colors::Yellow);

	ds->DrawTextLayout(out_layout, start_x + out_layout_xoff, y, Colors::Yellow);
	ds->FillRectangle(x, out_y - wingsize, this->length, this->arrowhead_size, this->pipe_brush);
	ds->DrawLine(start_x, out_y, end_x,                          out_y,            this->arrow_brush);
	ds->DrawLine(start_x, out_y, start_x + this->arrowhead_size, out_y - wingsize, this->arrow_brush);
	ds->DrawLine(start_x, out_y, start_x + this->arrowhead_size, out_y + wingsize, this->arrow_brush);
}
