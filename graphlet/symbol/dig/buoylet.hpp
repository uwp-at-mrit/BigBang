#include <fstream>

#include "turtle.hpp"

#include "graphlet/primitive.hpp"
#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private enum class BuoyType {
		_1, _2, _3, _4, _5, _6, _7, _8, _9, _10,
		White, Green, Red, BlackYellow,
		_
	};
	
	private struct BuoyDig : public IconDig {
	public:
		BuoyDig(std::filebuf& dig, WarGrey::SCADA::BuoyType subtype, float size);

	public:
		WarGrey::SCADA::IGraphlet* make_graphlet(double* x, double* y) override;
		Platform::String^ to_string() override;

	private:
		WarGrey::SCADA::BuoyType subtype;
		float size;
	};

	private struct BuoyStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	private struct Buoylet : public IStatelet<WarGrey::SCADA::BuoyType, WarGrey::SCADA::BuoyStyle> {
	public:
		Buoylet(WarGrey::SCADA::BuoyType subtype, float size);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		bool resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::BuoyType type, WarGrey::SCADA::BuoyStyle& style) override;
		void on_state_changed(WarGrey::SCADA::BuoyType type) override;

	private:
		void construct_buoy(bool resized);
		WarGrey::SCADA::GreenTurtle* make_colored_buoy_turtle(float width, float height);
		WarGrey::SCADA::GreenTurtle* make_buoy_mask_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ right_mask;

	private:
		float width;
		float height;
	};
}
