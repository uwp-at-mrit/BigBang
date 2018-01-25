#pragma once

#include "decorator/pipeline.hpp"
#include "snip/serew/serewsnip.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

PipelineDecorator::PipelineDecorator(bool draw_in, bool draw_out, bool draw_motor) {
    this->draw_inport = draw_in;
    this->draw_outport = draw_out;
	this->draw_motorport = draw_motor;
}

void PipelineDecorator::draw_after_snip(ISnip* self, CanvasDrawingSession^ ds, float x, float y, float width, float height) {
    if (this->draw_inport || this->draw_outport) {
       IPipeSnip* pipe = dynamic_cast<IPipeSnip*>(self);

       if (pipe != nullptr) {
           if (this->draw_inport) {
               static auto color = make_solid_brush(Colors::Orange);
               Rect region = pipe->get_input_port();
               ds->DrawRectangle(x + region.X, y + region.Y, region.Width, region.Height, color, 1.0F);
           }

           if (this->draw_outport) {
               static auto color = make_solid_brush(Colors::Firebrick);
               Rect region = pipe->get_output_port();
               ds->DrawRectangle(x + region.X, y + region.Y, region.Width, region.Height, color, 1.0F);
           }
       }
    }

	if (this->draw_motorport) {
		IMotorSnip* pipe = dynamic_cast<IMotorSnip*>(self);

		if (pipe != nullptr) {
			static auto color = make_solid_brush(Colors::ForestGreen);

			Rect region = pipe->get_motor_port();
			ds->DrawRectangle(x + region.X, y + region.Y, region.Width, region.Height, color, 1.0F);
		}
	}
}
