#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class PipelineDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        PipelineDecorator(bool draw_inport = true, bool draw_outport = true);

    public:
        void draw_after_snip(
            WarGrey::SCADA::ISnip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) override;

    private:
        bool draw_inport;
        bool draw_outport;
    };
}
