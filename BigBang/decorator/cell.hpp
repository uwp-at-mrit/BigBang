#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class CellDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
		CellDecorator(unsigned int color, const Windows::Foundation::Rect* src, size_t count, float radius = 8.0F);
		CellDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			const Windows::Foundation::Rect* src, size_t count, float radius = 8.0F);

		CellDecorator(unsigned int color, float width, float height, size_t count, size_t col, float gapsize = 2.0F, float radius = 8.0F);
		CellDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			float width, float height, size_t count, size_t col, float gapsize = 2.0F, float radius = 8.0F);

		template<class T, int N>
		CellDecorator(T color, const Windows::Foundation::Rect (&src)[N], float radius = 8.0F)
			: CellDecorator(color, src, N, radius) {}

    public:
        void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) override;

	public:
		void fill_cell_extent(unsigned int idx,
			float* x = nullptr, float* y = nullptr,
			float* width = nullptr, float* height = nullptr);

		void fill_cell_anchor(unsigned int idx, float fx, float fy, float* x = nullptr, float* y = nullptr);

	protected:
		~CellDecorator() noexcept;

    private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Windows::Foundation::Rect* boxes;
		float radius;
		size_t count;
    };
}
