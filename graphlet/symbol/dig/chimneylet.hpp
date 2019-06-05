#include "graphlet/primitive.hpp"

#include "turtle.hpp"

namespace WarGrey::SCADA {
	private class Chimneylet : public WarGrey::SCADA::IGraphlet {
	public:
		Chimneylet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_chimney(bool resized);
		WarGrey::SCADA::ITurtle* make_chimney_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		float width;
		float height;
	};
}
