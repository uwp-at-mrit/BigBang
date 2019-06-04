#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private class ReportSpotlet : public WarGrey::SCADA::IGraphlet {
	public:
		ReportSpotlet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		void construct_pilot_station(bool resized);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ rhombus;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ spot;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		float width;
		float height;
	};
}
