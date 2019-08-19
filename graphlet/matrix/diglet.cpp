#include <ppltasks.h>
#include <iostream>
#include <fstream>

#include "decorator/border.hpp"

#include "graphlet/matrix/diglet.hpp"
#include "graphlet/symbol/dig/dig.hpp"
#include "graphlet/symbol/dig/digmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
DigMap::DigMap() : lx(infinity), ty(infinity), rx(-infinity), by(-infinity) {
	this->cursor = this->items.end();
}

DigMap::~DigMap() {
	while (!this->items.empty()) {
		auto it = this->items.begin();
		
		delete (*it);
		
		this->items.erase(it);
	}
}

void DigMap::push_back_item(WarGrey::SCADA::IDigDatum* item) {
	this->items.push_back(item);
	this->counters[item->type] = this->counters[item->type] + 1;

	this->lx = flmin(this->lx, item->lx);
	this->rx = flmax(this->rx, item->rx);
	this->ty = flmin(this->ty, item->ty);
	this->by = flmax(this->by, item->by);
}

void DigMap::rewind() {
	this->cursor = this->items.end();
}

IDigDatum* DigMap::step() {
	IDigDatum* datum = nullptr;

	if (this->cursor == this->items.end()) {
		this->cursor = this->items.begin();
	} else {
		this->cursor++;
	}

	if (this->cursor != this->items.end()) {
		datum = (*this->cursor);
	}

	return datum;
}

void DigMap::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->lx, y, this->ty);
	SET_VALUES(width, this->rx - this->lx, height, this->by - this->ty);
}

IAsyncOperation<DigMap^>^ DigMap::load_async(Platform::String^ _dig) {
	return create_async([=] {
		DigMap^ map = nullptr;
		IDigDatum* datum;
		std::filebuf dig;

		if (dig.open(_dig->Data(), std::ios::in)) {
			map = ref new DigMap();

			while ((datum = read_dig_line(dig, 1600.0F)) != nullptr) {
				if (datum->type < DigDatumType::_) {
					map->push_back_item(datum);
				}
			}
		}

		return map;
	});
}

/*************************************************************************************************/
private class DigFrame : public Planet {
public:
	virtual ~DigFrame() noexcept {}

	DigFrame(Platform::String^ name) : Planet(name) {
		this->push_decorator(new BorderDecorator());
	}

public:
	bool can_select(IGraphlet* g) override {
		return true;
	}
};

/*************************************************************************************************/
Diglet::Diglet(Platform::String^ file, float view_width, float view_height, double scale, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new DigFrame(file), GraphletAnchor::LT, background)
	, view_width(view_width), view_height(view_height), origin_scale(scale) {
	this->ms_appdata_dig = ms_appdata_file(file, ".DIG", rootdir);
	this->enable_stretch(false, false);
}

Diglet::~Diglet() {
	this->unload(this->ms_appdata_dig);
}

void Diglet::construct() {
	Planetlet::construct();

	this->load(this->ms_appdata_dig, 0);
}

void Diglet::on_appdata(Uri^ ms_appdata, DigMap^ doc_dig, int hint) {
	doc_dig->fill_enclosing_box(&this->map_y, &this->map_x, &this->map_height, &this->map_width);
	
	this->planet->begin_update_sequence();

	{ // affine transformation
		double dvwidth = double(this->view_width) / this->origin_scale;
		double dvheight = double(this->view_height) / this->origin_scale;
		double tx = dvwidth * 0.5;
		double ty = dvheight * 0.5F;

		this->planet->scale(float(this->origin_scale));
		this->planet->translate(float(tx), float(ty));

		/** NOTE
		 *   For the sake of simplicity, non-icon items are organized as a batch.
		 *   Also, they are drawn before drawing icons.
		 */
		this->planet->insert(new DigMaplet(doc_dig, dvwidth, dvheight, tx, ty), -float(tx), -float(ty));
	}

	{ // make icons
		IDigDatum* datum = nullptr;
		double x, y;

		doc_dig->rewind();
		while ((datum = doc_dig->step()) != nullptr) {
			if (datum->type == DigDatumType::Icon) {
				IGraphlet* g = datum->make_graphlet(&x, &y);

				if (g != nullptr) {
					this->planet->insert(g, float(y), float(this->map_height - x), GraphletAnchor::LT);
				}
			}
		}
	}

	this->planet->end_update_sequence();

	this->graph_dig = doc_dig;
}

bool Diglet::ready() {
	return (this->graph_dig != nullptr);
}

void Diglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_width, h, this->view_height);
}

void Diglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Planetlet::draw(ds, x, y, Width, Height);
}

void Diglet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appdata_dig);

	draw_invalid_bitmap(hint, ds, x, y, Width, Height);
}
