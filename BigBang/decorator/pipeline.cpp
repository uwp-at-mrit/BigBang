#pragma once

#include "decorator/pipeline.hpp"
#include "snip/pipe/pipesnip.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

PipelineDecorator::PipelineDecorator(bool draw_in, bool draw_out) {
    this->draw_inport = draw_in;
    this->draw_outport = draw_out;
}

void PipelineDecorator::draw_after_snip(Snip* self, CanvasDrawingSession^ ds, float x, float y, float w, float h) {
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
}
