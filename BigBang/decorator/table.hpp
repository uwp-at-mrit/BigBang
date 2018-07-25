#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	private class ITableDecorator abstract : public virtual WarGrey::SCADA::IPlanetDecorator {
	public:
		ITableDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, size_t count, float radius);

	public:
		virtual void fill_cell_extent(unsigned int idx, float* x = nullptr, float* y = nullptr,
			float* width = nullptr, float* height = nullptr) = 0;

	public:
		unsigned int cell_count();
		int find_cell(float mouse_x, float mouse_y);
		void fill_cell_anchor(unsigned int idx, float fx, float fy, float* x = nullptr, float* y = nullptr);

	public:
		void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;

	protected:
		virtual void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, float radius,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		unsigned int count;
		float radius;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	private class TableDecorator : public WarGrey::SCADA::ITableDecorator {
	public:
		TableDecorator(unsigned int color, size_t count, size_t col, float hgap = 2.0F, float vgap = -1.0F, float radius = 8.0F);
		TableDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			size_t count, size_t col, float hgap = 2.0F, float vgap = -1.0F, float radius = 8.0F);

	public:
		void fill_cell_extent(unsigned int idx, float* x = nullptr, float* y = nullptr,
			float* width = nullptr, float* height = nullptr) override;

	private:
		float vgapsize;
		float hgapsize;
		size_t col;
		size_t row;
	};

	private class CellDecorator : public WarGrey::SCADA::ITableDecorator {
	public:
		~CellDecorator() noexcept;

    public:
		CellDecorator(unsigned int color, const Windows::Foundation::Rect* src, size_t count, float radius = 8.0F);
		CellDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			const Windows::Foundation::Rect* src, size_t count, float radius = 8.0F);

		template<class T, size_t N>
		CellDecorator(T color, Windows::Foundation::Rect (&src)[N], float radius = 8.0F)
			: CellDecorator(color, src, N, radius) {}

	public:
		void fill_cell_extent(unsigned int idx, float* x = nullptr, float* y = nullptr,
			float* width = nullptr, float* height = nullptr) override;

    private:
		Windows::Foundation::Rect* boxes;
    };
}
