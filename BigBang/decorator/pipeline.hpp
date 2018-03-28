#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class PipelineDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        PipelineDecorator(bool draw_inport = true, bool draw_outport = true);

    public:
        void draw_after_graphlet(
            WarGrey::SCADA::IGraphlet* g,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height, bool selected) override;

	protected:
		~PipelineDecorator() noexcept {}

    private:
        bool draw_inport;
        bool draw_outport;
    };
}
