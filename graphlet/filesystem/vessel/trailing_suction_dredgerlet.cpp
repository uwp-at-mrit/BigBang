#include <ppltasks.h>

#include "graphlet/filesystem/vessel/trailing_suction_dredgerlet.hpp"

#include "datum/flonum.hpp"
#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::System;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
TrailingSuctionDredgerlet::TrailingSuctionDredgerlet(Platform::String^ vessel, float real_width, float real_height, Platform::String^ ext, Platform::String^ rootdir)
	: real_width(real_width), real_height(real_height) {
	this->ms_appdata_config = ms_appdata_file(vessel, ext, rootdir);
}

TrailingSuctionDredgerlet::~TrailingSuctionDredgerlet() {
}

void TrailingSuctionDredgerlet::construct() {
	this->load(this->ms_appdata_config, 0);
}

void TrailingSuctionDredgerlet::on_appdata(Uri^ vessel, TrailingSuctionDredger^ vessel_config, int hint) {

}

bool TrailingSuctionDredgerlet::ready() {
	return (this->vessel_config != nullptr);
}

void TrailingSuctionDredgerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->real_width, h, this->real_height);
}

void TrailingSuctionDredgerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawRectangle(x, y, Width, Height,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		2.0F);
}

void TrailingSuctionDredgerlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	//Platform::String^ hint = file_name_from_path(this->ms_appdata_rootdir);

	//draw_invalid_bitmap(hint, ds, x, y, Width, Height);
}

/*************************************************************************************************/
IAsyncOperation<TrailingSuctionDredger^>^ TrailingSuctionDredger::load_async(Platform::String^ path) {
	return create_async([=] {
		TrailingSuctionDredger^ dredger = nullptr;

		return dredger;
	});
}
