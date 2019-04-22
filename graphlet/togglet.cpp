#include "graphlet/togglet.hpp"

#include "datum/box.hpp"

#include "text.hpp"
#include "tongue.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

Togglet::Togglet(bool initial_state, Platform::String^ checked_label, Platform::String^ unchecked_label, float width
	, CanvasSolidColorBrush^ ckcolor, CanvasSolidColorBrush^ uncolor)
	: state(initial_state), width(width), ckcolor(ckcolor), uncolor(uncolor) {
	CanvasTextFormat^ font = make_text_format("Consolas", 24.0F);

	this->label = make_text_layout(speak(checked_label), font);
	this->unlabel = make_text_layout(speak(unchecked_label), font);
	this->lblcolor = Colours::contrast(ckcolor);
	this->unlblcolor = Colours::contrast(uncolor);

	this->state = initial_state;
	this->width = width;
	
	if (this->width <= 0.0F) {
		float label_height = this->label->LayoutBounds.Height;

		if (width == 0.0F) {
			float label_width = std::max(this->label->LayoutBounds.Width, this->unlabel->LayoutBounds.Width);

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
		ds->FillRoundedRectangle(x, y, width, diameter, radius, radius, this->ckcolor);
		ds->DrawTextLayout(this->label, x + radius, y, this->lblcolor);
		ds->FillCircle(float2(x + width - radius, y + radius), bradius, this->lblcolor);
	} else {
		ds->FillRoundedRectangle(x, y, width, diameter, radius, radius, this->uncolor);
		ds->DrawTextLayout(this->unlabel, x + width - radius - this->unlabel->LayoutBounds.Width, y, this->unlblcolor);
		ds->FillCircle(float2(x + radius, y + radius), bradius, this->unlblcolor);
	}
}

bool Togglet::checked() {
	return this->state;
}

void Togglet::toggle() {
	this->state = !this->state;
	this->notify_updated();
}
