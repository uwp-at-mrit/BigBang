#pragma once

namespace WarGrey::SCADA {
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees, float cx, float cy);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_stroke(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, float thickness,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_translate(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, float tx, float ty = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_scale(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, float sx, float sy = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_subtract(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx = 0.0F, float ty = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx = 0.0F, float ty = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx = 0.0F, float ty = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx = 0.0F, float ty = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_subtract(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g0,
		float tx1, float ty1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g0,
		float tx1, float ty1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g0,
		float tx1, float ty1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g0,
		float tx1, float ty1,
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
		float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_subtract(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g,
		float tx1, float ty1, float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g,
		float tx1, float ty1, float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g,
		float tx1, float ty1, float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g,
		float tx1, float ty1, float tx2 = 0.0F, float ty2 = 0.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ gs[],
		size_t count);

	template<size_t N>
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry ^ (&gs)[N]) {
		return geometry_union(gs, N);
	};

/*************************************************************************************************/
	Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ geometry_freeze(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry);

	Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ geometry_draft(
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry,
		float linewidth = 1.0F);
}