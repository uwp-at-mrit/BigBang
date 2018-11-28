#include "graphlet/buttonlet.hpp"

#include "text.hpp"
#include "tongue.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ button_default_font = make_bold_text_format("Consolas", 16.0F);

Buttonlet::Buttonlet(Platform::String^ caption, float width, float height, float thickness, float corner_radius, Platform::String^ tongue)
	: Buttonlet(ButtonState::Default, caption, width, height, thickness, corner_radius, tongue) {}

Buttonlet::Buttonlet(ButtonState default_state, Platform::String^ caption, float width, float height
	, float thickness, float corner_radius, Platform::String^ tongue)
	: IStatelet(default_state), caption(speak(caption, tongue))
	, width(width), height(height), thickness(thickness)
	, corner_radius(corner_radius) {}

void Buttonlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void Buttonlet::prepare_style(ButtonState status, ButtonStyle& s) {
	switch (status) {
	case ButtonState::Executing: {
		CAS_SLOT(s.border_color, Colours::RoyalBlue);
		CAS_SLOT(s.background_color, Colours::SkyBlue);
	}; break;
	case ButtonState::Failed: {
		CAS_SLOT(s.border_color, Colours::Crimson);
		CAS_SLOT(s.background_color, Colours::LavenderBlush);
	}; break;
	case ButtonState::Ready: {
		CAS_SLOT(s.border_color, Colours::Green);
		CAS_SLOT(s.background_color, Colours::Honeydew);
	}; break;
	case ButtonState::Disabled: {
		CAS_SLOT(s.border_color, Colours::Gray);
		CAS_SLOT(s.background_color, Colours::LightGray);
		CAS_SLOT(s.foreground_color, Colours::GrayText);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::Gainsboro);
	CAS_SLOT(s.background_color, Colours::WhiteSmoke);
	CAS_SLOT(s.foreground_color, Colours::Black);
	CAS_SLOT(s.font, button_default_font);
}

void Buttonlet::on_status_changed(ButtonState status) {
	ButtonStyle s = this->get_style();

	this->label = make_text_layout(this->caption, s.font);
}

void Buttonlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ButtonStyle s = this->get_style();
	Rect box = this->label->DrawBounds;
	float cpt_x = x + (this->width - box.Width) * 0.5F;
	float cpt_y = y + (this->height - box.Height) * 0.5F;
	float btn_x = this->thickness;
	float btn_y = this->thickness;
	float btn_width = this->width - btn_x * 2.0F;
	float btn_height = this->height - btn_y * 2.0F;
	
	ds->FillRoundedRectangle(x + btn_x, y + btn_y, btn_width, btn_height,
		this->corner_radius, this->corner_radius,
		s.background_color);
	
	ds->DrawRoundedRectangle(x + btn_x, y + btn_y, btn_width, btn_height,
		this->corner_radius, this->corner_radius,
		s.border_color, this->thickness);

	ds->DrawTextLayout(this->label, cpt_x - box.X, cpt_y - box.Y, s.foreground_color);
}
