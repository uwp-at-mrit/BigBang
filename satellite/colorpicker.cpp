#include <map>

#include "satellite/colorpicker.hpp"

#include "palette/x11.hpp"
#include "palette/xterm256.hpp"

#include "datum/box.hpp"
#include "datum/string.hpp"

#include "text.hpp"
#include "tongue.hpp"
#include "brushes.hxx"
#include "colorspace.hpp"

using namespace WarGrey::DTPM;
using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static std::map<Palette, ColorPicker*> pickers;
static std::map<unsigned int, CanvasSolidColorBrush^> contrast_colors;

/*************************************************************************************************/
static CanvasSolidColorBrush^ contrast_color_ref(CanvasSolidColorBrush^ color) {
	unsigned int hex = color_to_hexadecimal(color->Color);
	auto maybe_color = contrast_colors.find(hex);

	if (maybe_color == contrast_colors.end()) {
		contrast_colors[hex] = Colours::contrast(color);
	}

	return contrast_colors[hex];
}

/*************************************************************************************************/
ColorPicker::ColorPicker(IPalette* palette) : ISatellite(Log::Info, palette->name()), palette(palette), gapsize(16.0F) {}

ColorPicker::~ColorPicker() {
	if (this->colors != nullptr) {
		delete[] this->colors;
	}
}

void ColorPicker::fill_extent(float* width, float* height) {
	float cell_width, cell_height;
	unsigned int col, row;

	this->palette->suggested_cell_layout(&col, &row);

	this->okay = new Buttonlet(ButtonState::Disabled, "_Okay");
	this->cancel = new Buttonlet(ButtonState::Ready, "_Cancel");

	this->okay->fill_extent(0.0F, 0.0F, &cell_width, &cell_height);

	cell_width *= 0.618F;
	cell_height *= 0.75F;

	SET_BOX(width, (cell_width + this->gapsize) * float(col) + this->gapsize);
	SET_BOX(height, (cell_height + this->gapsize) * float(row) + this->gapsize * 3.0F + cell_height);
}

void ColorPicker::load(CanvasCreateResourcesReason reason, float width, float height) {
	float cell_width, cell_height;
	unsigned int col, row;

	this->palette->suggested_cell_layout(&col, &row);
	this->okay->fill_extent(0.0F, 0.0F, &cell_width, &cell_height);

	cell_width = (width - this->gapsize) / float(col) - this->gapsize;
	cell_height = (height - cell_height - this->gapsize * 3.0F) / float(row) - this->gapsize;

	this->insert(this->okay);
	this->insert(this->cancel);

	this->colors = new Credit<Rectanglet, unsigned int> * [this->palette->capacity()];
	for (unsigned int idx = 0; idx < this->palette->capacity(); idx++) {
		this->colors[idx] = this->insert_one(new Credit<Rectanglet, unsigned int>(cell_width, cell_height, this->palette->color_ref(idx)), idx);
	}

	this->label = this->insert_one(new Labellet("000000", Colours::Transparent));
	this->label->camouflage(true);
}

void ColorPicker::reflow(float width, float height) {
	unsigned int col;

	this->palette->suggested_cell_layout(&col, nullptr);

	this->move_to(this->label, 0.0, 0.0, GraphletAnchor::RB);
	this->move_to(this->colors[0], this->gapsize, this->gapsize);

	for (unsigned int idx = 1; idx < this->palette->capacity(); idx++) {
		if (idx % col == 0) {
			this->move_to(this->colors[idx], this->colors[idx - col], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, this->gapsize);
		} else {
			this->move_to(this->colors[idx], this->colors[idx - 0x1], GraphletAnchor::RC, GraphletAnchor::LC, this->gapsize, 0.0F);
		}
	}

	this->move_to(this->okay, width - this->gapsize, height - this->gapsize, GraphletAnchor::RB);
	this->move_to(this->cancel, this->okay, GraphletAnchor::LC, GraphletAnchor::RC, -this->gapsize);
}

bool ColorPicker::can_select(IGraphlet* g) {
	return ((dynamic_cast<Credit<Rectanglet, unsigned int>*>(g) != nullptr)
		|| (dynamic_cast<Labellet*>(g) != nullptr)
		|| button_enabled(g));
}

void ColorPicker::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto c = dynamic_cast<Credit<Rectanglet, unsigned int>*>(g);
	
	if (c != nullptr) {
		auto brush = this->palette->color_ref(c->id);

		this->begin_update_sequence();

		this->label->set_text(L"%S", hexnumber(color_to_hexadecimal(brush->Color), 3U), GraphletAnchor::CC);
		this->label->set_color(contrast_color_ref(brush));
		this->move_to(this->label, g, GraphletAnchor::CC, GraphletAnchor::CC);

		this->selected_color = c;
		this->okay->set_state(ButtonState::Ready);
		this->label->camouflage(false);

		this->end_update_sequence();
	} else if ((this->okay == g) || (this->label == g)) {
		if (this->receiver != nullptr) {
			this->receiver->on_color_pick(this->palette->color_ref(this->selected_color->id), this->target);
		}

		this->hide();
	} else if (this->cancel == g) {
		this->hide();
	}
}

void ColorPicker::show(IColorPickerReceiver* receiver, IGraphlet* target) {
	this->receiver = receiver;
	this->target = target;

	ISatellite::show();
}

void ColorPicker::on_hiden() {
	this->selected_color = nullptr;
	this->receiver = nullptr;
	this->target = nullptr;

	this->label->camouflage(true);
	this->okay->set_state(ButtonState::Disabled);
}

ColorPicker* ColorPicker::get_instance(Palette ptype) {
	if (pickers.find(ptype) == pickers.end()) {
		IPalette* palette = nullptr;

		switch (ptype) {
		case Palette::X11:      palette = X11Palette::instance(); break;
		case Palette::Xterm256: palette = Xterm256Palette::instance(); break;
		}

		pickers.insert(std::pair<Palette, ColorPicker*>(ptype, new ColorPicker(palette)));
	}

	return pickers[ptype];
}
