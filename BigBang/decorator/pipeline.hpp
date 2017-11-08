#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class PipelineDecorator sealed : public WarGrey::SCADA::IUniverseDecorator {
    public:
        PipelineDecorator(bool draw_inport = true, bool draw_outport = true);

    public:
        void draw_after_snip(
            WarGrey::SCADA::Snip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) override;

    private:
        bool draw_inport;
        bool draw_outport;
    };
}
