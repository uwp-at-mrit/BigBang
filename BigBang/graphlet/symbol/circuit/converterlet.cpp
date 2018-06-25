#include "graphlet/symbol/circuit/converterlet.hpp"

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

/*************************************************************************************************/
Converterlet::Converterlet(Platform::String^ sign, float radius, float thickness, double degrees)
	: Converterlet(ConverterStatus::Normal, sign, radius, thickness, degrees) {}

Converterlet::Converterlet(ConverterStatus default_status, Platform::String^ sign, float radius, float thickness, double degrees)
	: ISymbollet(default_status, radius, degrees), thickness(thickness) {
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

	auto g_body = geometry_union(border, diagonal);
	auto g_sign = paragraph(l_sign)->Transform(make_translation_matrix(-s_box.X + s_x, -s_box.Y + s_y));
	auto g_pair = paragraph(l_pair)->Transform(make_translation_matrix(-p_box.X + p_x, -p_box.Y + p_y));

	this->body = geometry_freeze(geometry_rotate(g_body, degrees, radius, radius));
	this->sign = geometry_freeze(geometry_rotate(geometry_union(g_sign, g_pair), degrees, radius, radius));
}

void Converterlet::prepare_style(ConverterStatus status, ConverterStyle& s) {
	switch (status) {
	case ConverterStatus::Breakdown: CAS_SLOT(s.color, Colours::Firebrick); break;
	}

	CAS_SLOT(s.color, Colours::GhostWhite);
}

void Converterlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ConverterStyle style = this->get_style();
	
	ds->FillRectangle(x, y, this->size, this->size, Colours::Background);
	ds->DrawCachedGeometry(this->body, x, y, style.color);
	ds->DrawCachedGeometry(this->sign, x, y, style.color);
}
