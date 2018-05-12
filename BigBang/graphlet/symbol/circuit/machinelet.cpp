#include "graphlet/symbol/circuit/machinelet.hpp"

#include "math.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "transformation.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static MachineStatus default_machine_status = MachineStatus::Normal;
static CanvasSolidColorBrush^ default_border_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ default_sign_color = Colours::DimGray;

MachineStyle WarGrey::SCADA::make_default_machine_style(MachineStatus status) {
	MachineStyle s;

	s.border_color = default_border_color;
	s.sign_color = default_sign_color;

	switch (status) {
	case MachineStatus::Breakdown: s.border_color = Colours::Firebrick; s.sign_color = s.border_color; break;
	}

	return s;
}

/*************************************************************************************************/
Machinelet::Machinelet(MachineShape shape, Platform::String^ sign, float radius, float thickness, double degrees)
	: Machinelet(default_machine_status, shape, sign, radius, thickness, degrees) {
}

Machinelet::Machinelet(MachineStatus default_status, MachineShape shape, Platform::String^ sign, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_machine_style, radius, degrees), thickness(thickness), shape(shape) {
	auto g_body = blank();
	auto g_sign = blank();
	auto g_pair = blank();

	switch (this->shape) {
	case MachineShape::Circle: {
		auto l_sign = make_text_layout(sign, make_bold_text_format("Monospace", radius));
		auto l_pair = make_text_layout(L"~", make_bold_text_format("Cambria Math", radius * 2.0F));
		Rect s_box = l_sign->DrawBounds;
		Rect p_box = l_pair->DrawBounds;
		float s_x = radius * 1.0F - s_box.Width  * 0.5F;
		float s_y = radius * 0.7F - s_box.Height * 0.5F;
		float p_x = radius * 1.0F - p_box.Width  * 0.5F;
		float p_y = radius * 1.4F - p_box.Height * 0.5F;

		g_body = geometry_stroke(circle(radius, radius, radius - this->thickness * 0.5F), this->thickness);
		g_sign = paragraph(l_sign)->Transform(make_translation_matrix(-s_box.X + s_x, -s_box.Y + s_y));
		g_pair = paragraph(l_pair)->Transform(make_translation_matrix(-p_box.X + p_x, -p_box.Y + p_y));
	}; break;
	case MachineShape::Box: {
		auto l_sign = make_text_layout(sign, make_bold_text_format("Cambria Math", radius * 1.2F));
		auto l_pair = make_text_layout(L"=", make_bold_text_format("Cambria Math", radius * 1.2F));
		Rect s_box = l_sign->DrawBounds;
		Rect p_box = l_pair->DrawBounds;
		float s_x = radius * 0.5F - s_box.Width  * 0.5F + this->thickness;
		float s_y = radius * 0.5F - s_box.Height * 0.5F;
		float p_x = radius * 1.5F - p_box.Width  * 0.5F - this->thickness;
		float p_y = radius * 1.5F - p_box.Height * 0.5F;
		float offset = this->thickness * 0.5F;
		float sidelength = radius * 2.0F - this->thickness;
		auto diagonal = line(offset + sidelength, offset, offset, offset + sidelength, this->thickness);
		auto border = geometry_stroke(rectangle(offset, offset, sidelength, sidelength), this->thickness);

		g_body = geometry_union(border, diagonal);
		g_sign = paragraph(l_sign)->Transform(make_translation_matrix(-s_box.X + s_x, -s_box.Y + s_y));
		g_pair = paragraph(l_pair)->Transform(make_translation_matrix(-p_box.X + p_x, -p_box.Y + p_y));
	}; break;
	}

	this->body = geometry_freeze(geometry_rotate(g_body, degrees, radius, radius));
	this->sign = geometry_freeze(geometry_rotate(geometry_union(g_sign, g_pair), degrees, radius, radius));
}

void Machinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const MachineStyle style = this->get_style();
	ICanvasBrush^ bcolor = (style.border_color != nullptr) ? style.border_color : default_border_color;
	ICanvasBrush^ scolor = (style.sign_color != nullptr) ? style.sign_color : default_sign_color;
	
	ds->FillRectangle(x, y, this->size, this->size, Colours::Background);
	ds->DrawCachedGeometry(this->body, x, y, bcolor);
	ds->DrawCachedGeometry(this->sign, x, y, scolor);
}
