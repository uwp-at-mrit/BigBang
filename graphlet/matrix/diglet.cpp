#include <ppltasks.h>

#include "graphlet/matrix/diglet.hpp"

#include "datum/path.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
IAsyncOperation<DigVectorMap^>^ DigVectorMap::LoadAsync(Platform::String^ dig) {
	return create_async([=] {
		return ref new DigVectorMap();
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
