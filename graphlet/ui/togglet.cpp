#include "graphlet/ui/togglet.hpp"

#include "datum/box.hpp"
#include "datum/flonum.hpp"

#include "text.hpp"
#include "tongue.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Platform::String^ toggle_default_font_name = "Microsoft YaHei";
static const float toggle_default_font_size = 16.0F;

static CanvasSolidColorBrush^ toggle_default_checked_color = Colours::DodgerBlue;
static CanvasSolidColorBrush^ toggle_default_unchecked_color = Colours::SlateGray;

ToggleStyle WarGrey::SCADA::make_toggle_style(float fontsize, Colour^ checked_color, Colour^ unchecked_color) {
	ToggleStyle s;

	s.font = make_text_format(toggle_default_font_name, ((fontsize > 0.0F) ? fontsize : toggle_default_font_size));
	s.checked_color = ((checked_color == nullptr) ? toggle_default_checked_color : checked_color);
	s.unchecked_color = ((unchecked_color == nullptr) ? toggle_default_unchecked_color : unchecked_color);

	return s;
}

/**************************************************************************************************/
Togglet::Togglet(bool state0, Platform::String^ label, float width, Platform::String^ tongue)
	: Togglet(make_toggle_style(), state0, label, width, tongue) {}

Togglet::Togglet(ToggleStyle& style, bool state0, Platform::String^ label, float width, Platform::String^ tongue)
	: Togglet(style, state0, label, label, width, tongue) {}

Togglet::Togglet(bool initial_state, Platform::String^ checked_label, Platform::String^ unchecked_label, float width, Platform::String^ tongue)
	: Togglet(make_toggle_style(), initial_state, checked_label, unchecked_label, width, tongue) {}

Togglet::Togglet(ToggleStyle& style, bool initial_state, Platform::String^ checked_label, Platform::String^ unchecked_label, float width, Platform::String^ tongue)
	: state(initial_state), width(width), style(style) {
	this->prepare_style();

	this->label = make_text_layout(speak(checked_label, tongue), this->style.font);
	this->unlabel = make_text_layout(speak(unchecked_label, tongue), this->style.font);

	this->state = initial_state;
	this->width = width;
	
	if (this->width <= 0.0F) {
		float label_height = this->label->LayoutBounds.Height;

		if (width == 0.0F) {
			float label_width = flmax(this->label->LayoutBounds.Width, this->unlabel->LayoutBounds.Width);

			this->width = label_width + label_height * 1.618F;
		} else {
			this->width *= (-label_height);
		}
	}
}

void Togglet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->label->LayoutBounds.Height);
}

void Togglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float width, diameter, radius, bradius;
	
	this->fill_extent(x, y, &width, &diameter);
	radius = diameter * 0.5F;
	bradius = radius * 0.80F;

	if (this->state) {
		ds->FillRoundedRectangle(x, y, width, diameter, radius, radius, this->style.checked_color);
		ds->DrawTextLayout(this->label, x + radius, y, this->style.checked_label_color);
		ds->FillCircle(float2(x + width - radius, y + radius), bradius, this->style.checked_label_color);
	} else {
		ds->FillRoundedRectangle(x, y, width, diameter, radius, radius, this->style.unchecked_color);
		ds->DrawTextLayout(this->unlabel, x + width - radius - this->unlabel->LayoutBounds.Width, y, this->style.unchecked_label_color);
		ds->FillCircle(float2(x + radius, y + radius), bradius, this->style.unchecked_label_color);
	}
}

bool Togglet::checked() {
	return this->state;
}

void Togglet::toggle() {
	this->state = !this->state;
	this->notify_updated();
}

void Togglet::toggle(bool state) {
	if (this->state != state) {
		this->toggle();
	}
}

void Togglet::prepare_style() {
	CAS_SLOT(this->style.font, make_text_format(toggle_default_font_name, toggle_default_font_size));

	CAS_SLOT(this->style.checked_color, toggle_default_checked_color);
	CAS_SLOT(this->style.unchecked_color, toggle_default_unchecked_color);
	CAS_SLOT(this->style.checked_label_color, Colours::contrast(this->style.checked_color));
	CAS_SLOT(this->style.unchecked_label_color, Colours::contrast(this->style.unchecked_color));
}
