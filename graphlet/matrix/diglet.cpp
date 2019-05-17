#include <ppltasks.h>
#include <iostream>
#include <fstream>

#include "decorator/border.hpp"

#include "graphlet/matrix/diglet.hpp"
#include "graphlet/symbol/dig/dig.hpp"
#include "graphlet/textlet.hpp"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
DigVectorMap::DigVectorMap() : lx(infinity), ty(infinity), rx(-infinity), by(-infinity) {}

DigVectorMap::~DigVectorMap() {
	while (!this->items.empty()) {
		auto it = this->items.begin();
		
		delete (*it);
		
		this->items.erase(it);
	}
}

void DigVectorMap::push_back_item(WarGrey::SCADA::IDigDatum* item) {
	double x, y, width, height;

	this->items.push_back(item);
	this->counters[item->type] = this->counters[item->type] + 1;

	item->fill_enclosing_box(&x, &y, &width, &height);
	this->lx = flmin(this->lx, x);
	this->rx = flmax(this->rx, x + width);
	this->ty = flmin(this->ty, y);
	this->by = flmax(this->by, y + height);
}

void DigVectorMap::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->lx, y, this->ty);
	SET_VALUES(width, this->rx - this->lx, height, this->by - this->ty);
}

IAsyncOperation<DigVectorMap^>^ DigVectorMap::load_async(Platform::String^ _dig) {
	return create_async([=] {
		DigVectorMap^ map = ref new DigVectorMap();
		IDigDatum* datum;
		std::filebuf dig;

		if (dig.open(_dig->Data(), std::ios::in)) {
			while ((datum = read_dig(dig)) != nullptr) {
				if (datum->type < DigDatumType::_) {
					map->push_back_item(datum);
				}

				discard_this_line(dig);
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
	this->enable_resize(false, false);
}

Diglet::~Diglet() {
	this->unload(this->ms_appdata_dig);
}

void Diglet::construct() {
	this->load(this->ms_appdata_dig, 0);
	Planetlet::construct();
}

void Diglet::on_appdata(Uri^ ms_appdata, DigVectorMap^ doc_dig, int hint) {
	this->graph_dig = doc_dig;

	this->graph_dig->fill_enclosing_box(&this->map_x, &this->map_y, &this->map_width, &this->map_height);
	this->planet->insert(new Labellet(L"DIG(%f, %f, %f, %f)",
		this->map_x * this->origin_scale, this->map_y * this->origin_scale,
		this->map_width * this->origin_scale, this->map_height * this->origin_scale));
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
