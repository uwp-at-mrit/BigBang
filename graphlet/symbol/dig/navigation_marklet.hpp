#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private class NavigationMarklet : public WarGrey::SCADA::IGraphlet {
	public:
		NavigationMarklet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_mark_icon(bool resized);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;

	private:
		float width;
		float height;
	};
}
