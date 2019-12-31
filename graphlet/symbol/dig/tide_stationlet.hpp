#include "graphlet/primitive.hpp"

namespace WarGrey::DTPM {
	private class TideStationlet : public WarGrey::SCADA::IGraphlet {
	public:
		TideStationlet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;

	private:
		float width;
		float height;
	};
}
