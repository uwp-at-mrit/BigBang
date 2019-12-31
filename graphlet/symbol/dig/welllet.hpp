#include "graphlet/primitive.hpp"

#include "turtle.hpp"

namespace WarGrey::DTPM {
	private class OilWelllet : public WarGrey::SCADA::IGraphlet {
	public:
		OilWelllet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ rig_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ well_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_light_house(bool resized);
		WarGrey::SCADA::ITurtle* make_rig_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_well_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ rig;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ well;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ rig_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ well_color;

	private:
		float width;
		float height;
	};
}
