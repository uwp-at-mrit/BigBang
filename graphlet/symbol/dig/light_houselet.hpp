#include "graphlet/primitive.hpp"

#include "turtle.hpp"

namespace WarGrey::SCADA {
	private class LightHouselet : public WarGrey::SCADA::IGraphlet {
	public:
		LightHouselet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ light_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ house_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_light_house(bool resized);
		WarGrey::SCADA::ITurtle* make_light_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_house_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ light;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ house;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ light_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ house_color;

	private:
		float width;
		float height;
	};
}
