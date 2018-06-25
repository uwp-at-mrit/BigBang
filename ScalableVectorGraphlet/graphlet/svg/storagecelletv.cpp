#include "graphlet/svg/storagecelletv.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static StorageCellVStatus default_storagecellv_status = StorageCellVStatus::Normal;
static CanvasSolidColorBrush^ default_sign_color = Colours::LightSlateGray;
static CanvasSolidColorBrush^ default_body_color = Colours::SlateGray;

StorageCellVStyle WarGrey::SCADA::make_default_storagecellv_style(StorageCellVStatus status) {
	StorageCellVStyle s;

	s.sign_color = default_sign_color;
	s.body_color = default_body_color;

	switch (status) {
	case StorageCellVStatus::Breakdown: s.body_color = Colours::Firebrick; break;
	case StorageCellVStatus::Charge:    s.sign_color = Colours::Orange; break;
	case StorageCellVStatus::Discharge: s.sign_color = Colours::Green; break;
	}

	return s;
}

/*************************************************************************************************/
StorageCelletv::StorageCelletv(float width, float height)
	: StorageCelletv(default_storagecellv_status, width, height) {}

StorageCelletv::StorageCelletv(StorageCellVStatus default_status, float width, float height)
	: Svglet(default_status, &make_default_storagecellv_style, width, height) {}

Platform::String^ StorageCelletv::name() {
	return "StorageCell";
}

void StorageCelletv::update(long long count, long long interval, long long uptime) {
	if (this->ready()) {
		switch (this->get_status()) {
		case StorageCellVStatus::Charge: {
			StorageCellVStyle style = this->get_style();
			Colour^ sign_color = ((style.sign_color == nullptr) ? default_sign_color : style.sign_color);
			
			if (count % 2 == 0) {	
				this->set_anode_color(sign_color);
				this->set_cathode_color(default_body_color);
			} else {
				this->set_anode_color(default_sign_color);
				this->set_cathode_color(sign_color);
			}

			this->notify_updated();
		}; break;
		case StorageCellVStatus::Discharge: {
			StorageCellVStyle style = this->get_style();
			Colour^ sign_color = ((style.sign_color == nullptr) ? default_sign_color : style.sign_color);

			if (count % 2 != 0) {
				this->set_anode_color(sign_color);
				this->set_cathode_color(default_body_color);
			} else {
				this->set_anode_color(default_sign_color);
				this->set_cathode_color(sign_color);
			}

			this->notify_updated();
		}; break;
		}
	}
}

void StorageCelletv::apply_style(StorageCellVStyle& style) {
	CanvasSolidColorBrush^ sign_color = ((style.sign_color == nullptr) ? default_sign_color : style.sign_color);
	CanvasSolidColorBrush^ body_color = ((style.body_color == nullptr) ? default_body_color : style.body_color);

	this->set_body_color(body_color);
	this->set_seal_color(sign_color, body_color);
}

void StorageCelletv::set_body_color(Colour^ color) {
	this->set_fill_color("body", color);
	this->set_stroke_color("body", color);
}

void StorageCelletv::set_seal_color(Colour^ color, Colour^ shadow) {
	this->set_shape_color("seal", color);
	this->set_shape_color("seal_left", color);
	this->set_shape_color("seal_right", color);

	if (shadow != nullptr) {
		this->set_shape_color("seal_shadow", shadow);
	}
}

void StorageCelletv::set_anode_color(Colour^ color) {
	this->set_shape_color("anode", color);
	this->set_shape_color("anode_base", color);
}

void StorageCelletv::set_cathode_color(Colour^ color) {
	this->set_shape_color("cathode", color);
	this->set_shape_color("cathode_base", color);
}
