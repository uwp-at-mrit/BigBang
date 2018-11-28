#include "graphlet/svg/storagecelletv.hpp"

#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasSolidColorBrush^ default_sign_color = nullptr;

/*************************************************************************************************/
StorageCelletv::StorageCelletv(float width, float height)
	: StorageCelletv(StorageCellVState::Normal, width, height) {}

StorageCelletv::StorageCelletv(StorageCellVState default_state, float width, float height)
	: Svglet(default_state, width, height) {}

Platform::String^ StorageCelletv::name() {
	return "StorageCell";
}

void StorageCelletv::update(long long count, long long interval, long long uptime) {
	if (this->ready()) {
		switch (this->get_status()) {
		case StorageCellVState::Charge: {
			StorageCellVStyle style = this->get_style();
			
			if (count % 2 == 0) {	
				this->set_anode_color(style.sign_color);
				this->set_cathode_color(default_sign_color);
			} else {
				this->set_anode_color(default_sign_color);
				this->set_cathode_color(style.sign_color);
			}

			this->notify_updated();
		}; break;
		case StorageCellVState::Discharge: {
			StorageCellVStyle style = this->get_style();

			if (count % 2 != 0) {
				this->set_anode_color(style.sign_color);
				this->set_cathode_color(default_sign_color);
			} else {
				this->set_anode_color(default_sign_color);
				this->set_cathode_color(style.sign_color);
			}

			this->notify_updated();
		}; break;
		}
	}
}

void StorageCelletv::on_ready() {
	if (default_sign_color == nullptr) {
		default_sign_color = make_solid_brush(this->get_fill_color("seal"));
	}
}

void StorageCelletv::prepare_style(StorageCellVState status, StorageCellVStyle& style) {
	switch (status) {
	case StorageCellVState::Normal: CAS_SLOT(style.sign_color, Colours::WhiteSmoke); break;
	case StorageCellVState::Breakdown: CAS_SLOT(style.body_color, Colours::Firebrick); break;
	case StorageCellVState::Charge: CAS_SLOT(style.sign_color, Colours::Orange); break;
	case StorageCellVState::Discharge: CAS_SLOT(style.sign_color, Colours::Green); break;
	}

	CAS_SLOT(style.body_color, Colours::SlateGray);
	CAS_SLOT(style.sign_color, default_sign_color);
}

void StorageCelletv::apply_style(StorageCellVStyle& style) {
	this->set_body_color(style.body_color);

	this->set_anode_color(style.sign_color);
	this->set_cathode_color(style.sign_color);
	this->set_seal_color(style.sign_color, style.body_color);
}

void StorageCelletv::set_body_color(Colour^ color) {
	this->set_fill_color("body", color);
	this->set_stroke_color("body", color);
}

void StorageCelletv::set_seal_color(Colour^ color, Colour^ shadow) {
	this->set_shape_color("seal", color);
	this->set_shape_color("seal-left", color);
	this->set_shape_color("seal-right", color);

	if (shadow != nullptr) {
		this->set_shape_color("seal-shadow", shadow);
	}
}

void StorageCelletv::set_anode_color(Colour^ color) {
	this->set_shape_color("anode", color);
	this->set_shape_color("anode-base", color);
}

void StorageCelletv::set_cathode_color(Colour^ color) {
	this->set_shape_color("cathode", color);
	this->set_shape_color("cathode-base", color);
}
