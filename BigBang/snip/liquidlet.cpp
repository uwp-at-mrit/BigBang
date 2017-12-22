#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "colorspace.hpp"
#include "snip/liquidlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Liquidlet::Liquidlet(float length, ArrowPosition position, double color, double saturation, double lightness) {
	this->length = length;
	this->position = position;
	this->arrowhead_size = 4.0F;
	this->arrow_brush = make_solid_brush(hsla(color, saturation, lightness, 1.00));
	this->pipe_brush = make_solid_brush(hsla(color, saturation, lightness, 0.40));
	this->font = make_text_format(8.0F);
}

void Liquidlet::load() {
	auto ts = get_text_extent("100.000", this->font);
	this->scale_width = ts.width;
	this->scale_height = ts.height;
	this->arrow_size = this->scale_width + this->arrowhead_size * 2.5F;
	this->height = this->scale_height * 2.0F + this->arrowhead_size * 1.5F;

	this->arrow = geometry_freeze(double_arrow(this->arrow_size, this->arrowhead_size));
}

void Liquidlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->length, h, this->height);
};

void Liquidlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	CanvasTextLayout^ in_scale = make_text_layout(this->in_temperature.ToString(), this->font);
	CanvasTextLayout^ out_scale = make_text_layout(this->out_temperature.ToString(), this->font);
	float pipe_height = this->height - this->scale_height * 2.0F;
	float in_scale_xoff = this->arrowhead_size + (this->scale_width - in_scale->LayoutBounds.Width) * 0.5F;
	float out_scale_xoff = this->arrowhead_size + (this->scale_width - out_scale->LayoutBounds.Width) * 0.5F;
	float arrow_xoff = 2.0F;
	float arrow_x = x + (this->position == ArrowPosition::Start) ? arrow_xoff : (this->length - this->arrow_size - arrow_xoff);
	float arrow_y = y + this->scale_height;

	ds->DrawTextLayout(out_scale, arrow_x + out_scale_xoff, arrow_y + pipe_height, Colors::Yellow);
	ds->FillRectangle(x, arrow_y, this->length, pipe_height, this->pipe_brush);
	ds->DrawCachedGeometry(this->arrow, arrow_x, arrow_y, this->arrow_brush);
	ds->DrawTextLayout(in_scale, arrow_x + in_scale_xoff, y, Colors::Yellow);
}
