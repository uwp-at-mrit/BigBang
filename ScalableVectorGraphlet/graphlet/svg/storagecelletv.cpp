#include "graphlet/svg/storagecelletv.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static StorageCellVStatus default_storagecellv_status = StorageCellVStatus::Normal;
static CanvasSolidColorBrush^ default_body_color = Colours::DarkGray;

StorageCellVStyle WarGrey::SCADA::make_default_storagecellv_style(StorageCellVStatus status) {
	StorageCellVStyle s;

	s.body_color = default_body_color;

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
	this->set_fill_color("body", style.body_color);
}
