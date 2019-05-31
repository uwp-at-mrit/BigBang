#include "graphlet/primitive.hpp"
#include "turtle.hpp"

namespace WarGrey::SCADA {
	private struct SunkenShiplet : public WarGrey::SCADA::IGraphlet{
	public:
		SunkenShiplet(float size,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_sunken_ship(bool resized);
		WarGrey::SCADA::ITurtle* make_sunken_ship_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color;

	private:
		float width;
		float height;
	};

	private struct Wrecklet : public WarGrey::SCADA::IGraphlet{
	public:
		Wrecklet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_wreck(bool resized);
		WarGrey::SCADA::ITurtle* make_wreck_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		float width;
		float height;
	};
}
