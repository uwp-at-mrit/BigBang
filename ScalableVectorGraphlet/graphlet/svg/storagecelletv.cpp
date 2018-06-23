#include "graphlet/svg/storagecelletv.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static StorageCellVStatus default_storagecellv_status = StorageCellVStatus::Normal;
static CanvasSolidColorBrush^ default_border_color = Colours::LightSlateGray;
static CanvasSolidColorBrush^ default_body_color = Colours::SlateGray;

StorageCellVStyle WarGrey::SCADA::make_default_storagecellv_style(StorageCellVStatus status) {
	StorageCellVStyle s;

	s.border_color = default_border_color;
	s.body_color = default_body_color;

	switch (status) {
	case StorageCellVStatus::Breakdown: s.border_color = Colours::Firebrick; break;
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

void StorageCelletv::apply_style(StorageCellVStyle& style) {
	CanvasSolidColorBrush^ border_color = ((style.border_color == nullptr) ? default_border_color : style.border_color);
	CanvasSolidColorBrush^ body_color = ((style.body_color == nullptr) ? default_body_color : style.body_color);
	
	this->set_body_color(border_color);

	this->set_anode_color(Colours::ForestGreen);
	this->set_cathode_color(Colours::DodgerBlue);
	this->set_seal_color(Colours::Yellow);
}

void StorageCelletv::set_body_color(Colour^ color, Colour^ shadow) {
	this->set_fill_color("body", color);
	this->set_stroke_color("body", Colours::Firebrick);

	this->set_shape_color("seal_shadow", ((shadow == nullptr) ? color : shadow));
}

void StorageCelletv::set_seal_color(Colour^ color) {
	this->set_shape_color("seal", color);
	this->set_shape_color("seal_left", color);
	this->set_shape_color("seal_right", color);
}

void StorageCelletv::set_anode_color(Colour^ color) {
	this->set_shape_color("anode", color);
	this->set_shape_color("anode_base", color);
}

void StorageCelletv::set_cathode_color(Colour^ color) {
	this->set_shape_color("cathode", color);
	this->set_shape_color("cathode_base", color);
}
