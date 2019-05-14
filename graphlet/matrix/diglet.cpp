#include <ppltasks.h>
#include <iostream>
#include <fstream>

#include "graphlet/matrix/diglet.hpp"
#include "graphlet/symbol/dig/dig.hpp"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
DigVectorMap::DigVectorMap() {}

DigVectorMap::~DigVectorMap() {
	while (!this->items.empty()) {
		auto it = this->items.begin();
		
		delete (*it);
		
		this->items.erase(it);
	}
}

void DigVectorMap::push_back_item(WarGrey::SCADA::IDigDatum* item) {
	double cx, cy, radius;

	this->items.push_back(item);
	this->counters[item->type] = this->counters[item->type] + 1;

	syslog(Log::Info, L"%s: %d", item->to_string()->Data(), this->counters[item->type]);

	item->fill_polar_extent(&cx, &cy, &radius);
	this->lx = flmin(this->lx, cx - radius);
	this->rx = flmax(this->rx, cx + radius);
	this->ty = flmin(this->ty, cy - radius);
	this->by = flmax(this->by, cy + radius);
}

IAsyncOperation<DigVectorMap^>^ DigVectorMap::LoadAsync(Platform::String^ _dig) {
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

				read_skip_this_line(dig);
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
	this->graph_dig = doc_dig;
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
