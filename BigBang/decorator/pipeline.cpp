#pragma once

#include "decorator/pipeline.hpp"
#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;

PipelineDecorator::PipelineDecorator(bool draw_in, bool draw_out) {
    this->draw_inport = draw_in;
    this->draw_outport = draw_out;
}

void PipelineDecorator::draw_after_graphlet(IGraphlet* self, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
    if (this->draw_inport || this->draw_outport) {
       IPipelet* pipe = dynamic_cast<IPipelet*>(self);

       if (pipe != nullptr) {
           if (this->draw_inport) {
               Rect region = pipe->get_input_port();
               ds->DrawRectangle(x + region.X, y + region.Y, region.Width, region.Height, Colours::Orange, 1.0F);
           }

           if (this->draw_outport) {
               Rect region = pipe->get_output_port();
               ds->DrawRectangle(x + region.X, y + region.Y, region.Width, region.Height, Colours::Firebrick, 1.0F);
           }
       }
    }
}
