#pragma once

namespace WarGrey::SCADA {
	typedef Platform::Array<Microsoft::Graphics::Canvas::Brushes::CanvasGradientStop> GradientStops;

	Windows::UI::Color gradient_discrete_color(WarGrey::SCADA::GradientStops^ stops, float percentage);
	Windows::UI::Color gradient_discrete_color(WarGrey::SCADA::GradientStops^ stops, double percentage);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(unsigned int hexes[], size_t total);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(unsigned int hexes[], float positions[], size_t total);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(Windows::UI::Color colors[], size_t total);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(Windows::UI::Color colors[], float positions[], size_t total);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ brushes[], size_t total);
	WarGrey::SCADA::GradientStops^ make_gradient_stops(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ brushes[], float positions[], size_t total);

	template<class T, size_t N>
	WarGrey::SCADA::GradientStops^ make_gradient_stops(T(&hints)[N]) {
		return make_gradient_stops(hints, N);
	};

	template<class T, size_t N>
	WarGrey::SCADA::GradientStops^ make_gradient_stops(T(&hints)[N], float(&positions)[N]) {
		return make_gradient_stops(hints, positions, N);
	};

	void brush_translate(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ brush, float x, float y);

	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ make_solid_brush(Windows::UI::Color& color, double alpha = 1.0);
	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ make_solid_brush(unsigned int hex, double alpha = 1.0);

	Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
		float hextent, float vextent, Windows::UI::Color& src_color,
		float light_scale = 1.618F, float dark_scale = 0.618F,
		Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
		Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);

	Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
		float hextent, float vextent, WarGrey::SCADA::GradientStops^ stops,
		Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
		Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);

	Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
		float start_x, float start_y, float end_x, float end_y, WarGrey::SCADA::GradientStops^ stops,
		Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
		Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);

	Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_dash_stroke(
		float* dashes, unsigned int count, float offset = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_dash_stroke(
		Microsoft::Graphics::Canvas::Geometry::CanvasDashStyle style, float offset = 0.0F);

	template<unsigned int N>
	Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_dash_stroke(float(&dashes)[N], float offset = 0.0F) {
		return WarGrey::SCADA::make_dash_stroke(dashes, N, offset);
	}

	Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_roundcap_stroke_style(bool shared = true);
	Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_round_stroke_style(bool shared = true);
}
