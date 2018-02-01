#include "numpadlet.hpp"

#include "planet.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Numpadlet::Numpadlet(float cell_size) {
	this->shown = false;
}

void Numpadlet::construct() {

}

void Numpadlet::fill_extent(float x, float y, float* w, float* h) {
	if (this->shown) {
		SET_VALUES(w, 128.0F, h, 128.0F);
	} else {
		SET_VALUES(w, 0.0F, h, 0.0F);
	}
}

void Numpadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->shown) {
		ds->FillRectangle(x, y, Width, Height, system_highlight_brush());
	}
}

void Numpadlet::show(ISnip* target_snip, float xoff, float yoff) {
	this->info->master->move_to(this, xoff, yoff);
	this->shown = true;
}
