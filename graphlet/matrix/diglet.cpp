#include <ppltasks.h>
#include <iostream>
#include <fstream>

#include "graphlet/matrix/diglet.hpp"
#include "graphlet/symbol/dig/dig.hpp"
#include "graphlet/textlet.hpp"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
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

	syslog(Log::Info, L"%s: %d", item->to_string()->Data(), this->counters[item->type]);

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
				} else {
					syslog(Log::Warning, L"Unrecognized type: %s", datum->name->Data());
				}

				discard_this_line(dig);
			}
		}

		return map;
	});
}

/*************************************************************************************************/
Diglet::Diglet(Platform::String^ file, GraphletAnchor anchor, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(nullptr, anchor, background) {
	this->ms_appdata_dig = ms_appdata_file(file, ".DIG", rootdir);
}

Diglet::~Diglet() {
	this->unload(this->ms_appdata_dig);
}

void Diglet::construct() {
	this->load(this->ms_appdata_dig, 0);
}

void Diglet::on_appdata(Uri^ ms_appdata, DigVectorMap^ doc_dig, int hint) {
	double x, y, width, height;

	this->graph_dig = doc_dig;

	this->graph_dig->fill_enclosing_box(&x, &y, &width, &height);
	this->planet->insert(new Labellet(L"DIG(%f, %f, %f, %f)", x, y, width, height));
}

bool Diglet::ready() {
	return (this->graph_dig != nullptr);
}

void Diglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->window.Width, h, this->window.Height);
}

void Diglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	this->window.X = x;
	this->window.Y = y;

	//ds->DrawImage(this->graph_dig, this->window);
}

void Diglet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appdata_dig);
}
