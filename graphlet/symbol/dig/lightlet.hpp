#include "graphlet/primitive.hpp"

namespace WarGrey::DTPM {
	private class Lightlet : public WarGrey::SCADA::IGraphlet {
	public:
		Lightlet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr
			, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ reflection_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_light(bool resized);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ reflection;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ reflection_color;

	private:
		float width;
		float height;
	};
}
