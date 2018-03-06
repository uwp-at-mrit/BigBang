#include "pipelinelet.hpp"

#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;


Pipelinelet::Pipelinelet(Platform::Array<PipeMove>^ moves, float usize, float thickness, Color& color)
	: Pipelinelet(0.0F, 0.0F, moves, usize, thickness, color) { }

Pipelinelet::Pipelinelet(float x, float y, Platform::Array<PipeMove>^ moves, float usize, float thickness, Color& color)
	: thickness(thickness) {
	auto pipes = ::pipeline(moves, usize, thickness);
	auto rect = pipes->ComputeStrokeBounds(1.0F);

	this->pipeline = geometry_freeze(pipes);
	this->color = make_solid_brush(color);

	this->width = rect.Width;
	this->height = rect.Height;
}

void Pipelinelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Pipelinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float adjust_offset = this->thickness * 0.5F;
	
	ds->DrawCachedGeometry(this->pipeline, x + adjust_offset, y + adjust_offset, this->color);
}
