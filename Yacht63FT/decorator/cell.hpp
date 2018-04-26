#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class CellDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
		CellDecorator(unsigned int color, const Windows::Foundation::Rect* src, size_t count, float radius = 4.0F);
		CellDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			const Windows::Foundation::Rect* src, size_t count, float radius = 4.0F);

		template<class T, int N>
		CellDecorator(T color, const Windows::Foundation::Rect (&src)[N], float radius = 4.0F)
			: CellDecorator(color, src, N, radius) {}

    public:
        void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) override;

	protected:
		~CellDecorator() noexcept;

    private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Windows::Foundation::Rect* boxes;
		float radius;
		size_t count;
    };
}
