#include "graphlet/filesystem/vessel/trailing_suction_dredgerlet.hpp"

#include "datum/flonum.hpp"
#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "brushes.hxx"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
TrailingSuctionDredgerlet::TrailingSuctionDredgerlet(Platform::String^ vessel, Platform::String^ ext, Platform::String^ rootdir) {
	if (vessel != nullptr) {
		this->ms_appdata_config = ms_appdata_file(vessel, ext, rootdir);
	} else {
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("trailing_suction_dredger", ext));
	}
}

TrailingSuctionDredgerlet::~TrailingSuctionDredgerlet() {
}

void TrailingSuctionDredgerlet::construct() {
	this->load(this->ms_appdata_config);
}

void TrailingSuctionDredgerlet::on_appdata(Uri^ vessel, TrailingSuctionDredger^ vessel_config) {
	this->preview_config->fill_boundary(nullptr, nullptr, &this->real_width, &this->real_height);

	this->vessel_config = vessel_config;
	this->preview_config = this->vessel_config;
}

bool TrailingSuctionDredgerlet::ready() {
	return (this->vessel_config != nullptr);
}

void TrailingSuctionDredgerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, float(this->real_width));
	SET_BOX(h, float(this->real_height));
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
void TrailingSuctionDredgerlet::preview(TrailingSuctionDredger^ src) {
	if (this->preview_config == nullptr) {
		this->preview_config = ref new TrailingSuctionDredger(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->preview_config->fill_boundary(nullptr, nullptr, &this->real_width, &this->real_height);
}

void TrailingSuctionDredgerlet::refresh(TrailingSuctionDredger^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
TrailingSuctionDredger^ TrailingSuctionDredger::load(Platform::String^ path) {
	TrailingSuctionDredger^ dredger = nullptr;

	return dredger;
}

bool TrailingSuctionDredger::save(TrailingSuctionDredger^ self, Platform::String^ path) {
	return true;
}

TrailingSuctionDredger::TrailingSuctionDredger(TrailingSuctionDredger^ src) {
	if (src != nullptr) {
		this->refresh(src);
	}
}

void TrailingSuctionDredger::refresh(TrailingSuctionDredger^ src) {
	if (src != nullptr) {
		size_t ptsize = sizeof(double2);

		this->ps_suction = src->ps_suction;
		this->sb_suction = src->sb_suction;
		this->trunnion = src->trunnion;
		this->barge = src->barge;

		for (size_t idx = 0; idx < sizeof(this->gps) / ptsize; idx++) {
			this->gps[idx] = src->gps[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->body_vertexes) / ptsize; idx++) {
			this->body_vertexes[idx] = src->body_vertexes[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->hopper_vertexes) / ptsize; idx++) {
			this->hopper_vertexes[idx] = src->hopper_vertexes[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->bridge_vertexes) / ptsize; idx++) {
			this->bridge_vertexes[idx] = src->bridge_vertexes[idx];
		}
	}
}

void TrailingSuctionDredger::fill_boundary(double* x, double* y, double* width, double* height) {
	size_t ptsize = sizeof(double2);
	double lx = this->ps_suction.x;
	double ty = this->ps_suction.y;
	double rx = this->ps_suction.x;
	double by = this->ps_suction.y;

	region_fuse_point(&lx, &ty, &rx, &by, this->sb_suction.x, this->sb_suction.y);
	region_fuse_point(&lx, &ty, &rx, &by, this->trunnion.x, this->trunnion.y);
	region_fuse_point(&lx, &ty, &rx, &by, this->barge.x, this->barge.y);

	for (size_t idx = 0; idx < sizeof(this->body_vertexes) / ptsize; idx++) {
		region_fuse_point(&lx, &ty, &rx, &by, this->body_vertexes[idx].x, this->body_vertexes[idx].y);
	}

	for (size_t idx = 0; idx < sizeof(this->bridge_vertexes) / ptsize; idx++) {
		region_fuse_point(&lx, &ty, &rx, &by, this->bridge_vertexes[idx].x, this->bridge_vertexes[idx].y);
	}

	SET_VALUES(x, lx, y, ty);
	SET_VALUES(width, rx - lx, height, by - ty);
}
