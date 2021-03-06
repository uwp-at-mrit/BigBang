#include <fstream>

#include "turtle.hpp"

#include "graphlet/primitive.hpp"
#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::DTPM {
	private enum class BuoyType {
		_1, _2, _3, _4, _5, _6, _7,
		RedWhite, Yellow, Black,
		White, Green, Red, BlackYellow,
		_
	};
	
	private struct BuoyDig : public IconDig {
	public:
		BuoyDig(std::filebuf& dig, WarGrey::DTPM::BuoyType subtype, float size);

	public:
		WarGrey::SCADA::IGraphlet* make_graphlet(double* x, double* y) override;
		Platform::String^ to_string() override;

	private:
		WarGrey::DTPM::BuoyType subtype;
		float size;
	};

	private struct BuoyStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color = nullptr;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color = nullptr;
	};

	private struct Buoylet : public WarGrey::SCADA::IStatelet<WarGrey::DTPM::BuoyType, WarGrey::DTPM::BuoyStyle> {
	public:
		Buoylet(WarGrey::DTPM::BuoyType subtype, float size);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::DTPM::BuoyType type, WarGrey::DTPM::BuoyStyle& style) override;
		void on_state_changed(WarGrey::DTPM::BuoyType type) override;

	private:
		void construct_buoy(bool resized);
		WarGrey::SCADA::ITurtle* make_colored_buoy_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_mask_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type1_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type2_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type3_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type4_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type5_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type6_turtle(float width, float height);
		WarGrey::SCADA::ITurtle* make_buoy_type7_turtle(float width, float height);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ right_mask;

	private:
		float width;
		float height;
	};
}
