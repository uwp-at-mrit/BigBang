#include <algorithm>
#include <shared_mutex>

#include "graphlet/datalet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "system.hpp"
#include "tongue.hpp"
#include "planet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ status_font = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

/*************************************************************************************************/
IDataViewlet::IDataViewlet(IDataProvider* dsrc, float width, float height) : datasource(dsrc), width(width), height(height) {
	if (this->datasource != nullptr) {
		this->datasource->reference();
	}
}

IDataViewlet::~IDataViewlet() {
	if (this->datasource != nullptr) {
		this->datasource->destroy();
	}
}

void IDataViewlet::fill_extent(float x, float y, float* width, float* height) {
	if (this->info != nullptr) {
		if (this->width <= 0.0F) {
			this->info->master->fill_actual_extent(&this->width, nullptr);
			this->width -= x;
		}

		if (this->height <= 0.0F) {
			this->info->master->fill_actual_extent(nullptr, &this->height);
			this->height -= y;
		}
	}

	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

/*************************************************************************************************/
ListViewlet::ListViewlet(IDataProvider* datasource, float width, float height)
	: IDataViewlet(datasource, width, height) {}

void ListViewlet::construct() {

}

void ListViewlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {

}
