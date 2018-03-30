#pragma once

#include "forward.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class IPlanetDecorator abstract : public WarGrey::SCADA::SharedObject {
    public:
        virtual void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_after(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_before_graphlet(
            WarGrey::SCADA::IGraphlet* g,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height, bool selected) {};

        virtual void draw_after_graphlet(
            WarGrey::SCADA::IGraphlet* g,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height, bool selected) {};

	protected:
		~IPlanetDecorator() noexcept {}
	};

	private class CompositeDecorator final : public WarGrey::SCADA::IPlanetDecorator {
	public:
		CompositeDecorator(IPlanetDecorator* first, IPlanetDecorator* second);
		CompositeDecorator(IPlanetDecorator** decorators, unsigned int count);

		template<unsigned int N>
		CompositeDecorator(IPlanetDecorator* (&decorators)[N]) : CompositeDecorator(decorators, N) {}

	public:
		void draw_before(
			WarGrey::SCADA::IPlanet* master,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float Width, float Height) override;

		void draw_after(
			WarGrey::SCADA::IPlanet* master,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float Width, float Height) override;

		void draw_before_graphlet(
			WarGrey::SCADA::IGraphlet* g,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) override;

		void draw_after_graphlet(
			WarGrey::SCADA::IGraphlet* g,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) override;

	protected:
		~CompositeDecorator() noexcept;

	private:
		WarGrey::SCADA::IPlanetDecorator** decorators;
		unsigned int count;
	};
}
