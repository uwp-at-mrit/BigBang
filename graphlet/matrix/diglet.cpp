#include <ppltasks.h>
#include <iostream>
#include <fstream>

#include "graphlet/matrix/diglet.hpp"
#include "graphlet/textlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"
#include "graphlet/symbol/dig/digmaplet.hpp"

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
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

namespace {
	private class DigFrame : public Planet {
	public:
		virtual ~DigFrame() noexcept {}

		DigFrame(Platform::String^ name) : Planet(name) {}

	public:
		bool can_select(IGraphlet* g) override {
			return true;
		}
	};

	private ref struct DigIconEntity sealed {
	internal:
		DigIconEntity(IGraphlet* icon, double x, double y, float width, float height)
			: icon(icon), x(x), y(y), width(width), height(height) {}

	internal:
		IGraphlet* icon;
		double x;
		double y;
		float width;
		float height;
	};


	static float2 icon_location(DigIconEntity^ icon, DigMaplet* map, float scale) {
		float2 ipos = map->position_to_local(icon->x, icon->y);
		float canvas_x = ipos.x / scale;
		float canvas_y = ipos.y / scale;
		
		/** WARNING
		* The modifyDIG does not handle rectangular items accurately.
		* Icons as well as rectangles should be translated vertically
		*   since modifyDIG uses the lefthand coordinate system.
		*/

		/** NOTE
		* Unlike Diglet, modifyDIG draws icons on the air,
		*   which means icons are technically dot-based items,
		*   thus, icons in modifyDIG are not affected by that bug.
		*
		* Also see DigMaplet::draw for DigDatumType::Rectangle.
		*/

		return float2(canvas_x - icon->width * 0.5F, canvas_y - icon->height);
	}
}

/*************************************************************************************************/
DigMap::DigMap() : lx(infinity), ty(infinity), rx(-infinity), by(-infinity) {
	this->cursor = this->items.end();
}

DigMap::~DigMap() {
	while (!this->items.empty()) {
		auto it = this->items.begin();
		
		delete (*it);
		
		this->items.erase(it);
	}
}

void DigMap::push_back_item(WarGrey::SCADA::IDigDatum* item) {
	this->items.push_back(item);
	this->counters[item->type] = this->counters[item->type] + 1;

	this->lx = flmin(this->lx, item->lx);
	this->rx = flmax(this->rx, item->rx);
	this->ty = flmin(this->ty, item->ty);
	this->by = flmax(this->by, item->by);
}

void DigMap::rewind() {
	this->cursor = this->items.end();
}

IDigDatum* DigMap::step() {
	IDigDatum* datum = nullptr;

	if (this->cursor == this->items.end()) {
		this->cursor = this->items.begin();
	} else {
		this->cursor++;
	}

	if (this->cursor != this->items.end()) {
		datum = (*this->cursor);
	}

	return datum;
}

void DigMap::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->lx, y, this->ty);
	SET_VALUES(width, this->rx - this->lx, height, this->by - this->ty);
}

IAsyncOperation<DigMap^>^ DigMap::load_async(Platform::String^ _dig) {
	return create_async([=] {
		DigMap^ map = nullptr;
		IDigDatum* datum;
		std::filebuf dig;

		if (dig.open(_dig->Data(), std::ios::in)) {
			map = ref new DigMap();

			while ((datum = read_dig_line(dig, 1600.0F)) != nullptr) {
				if (datum->type < DigDatumType::_) {
					map->push_back_item(datum);
				}
			}
		}

		return map;
	});
}

/*************************************************************************************************/
Diglet::Diglet(Platform::String^ file, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new DigFrame(file), GraphletAnchor::LT, background), view_width(view_width), view_height(view_height), map(nullptr) {
	this->ms_appdata_dig = ms_appdata_file(file, ".DIG", rootdir);
	this->enable_stretch(false, false);
	this->enable_events(true, true);
}

Diglet::~Diglet() {
	this->unload(this->ms_appdata_dig);
}

void Diglet::construct() {
	Planetlet::construct();

	this->load(this->ms_appdata_dig, 0);
}

void Diglet::on_appdata(Uri^ ms_appdata, DigMap^ doc_dig, int hint) {
	DigMaplet* map = new DigMaplet(doc_dig, this->view_width, this->view_height);
	float initial_scale = float(map->scale());
	
	this->map = map;
	this->planet->begin_update_sequence();
	this->planet->scale(initial_scale);
		
	/** NOTE
	 * For the sake of simplicity, non-icon items are organized as a batch.
	 * Also, they are drawn before drawing icons.
	 *
	 * The modifyDIG draw icons firstly.
	 */
	this->planet->insert(map, 0.0F, 0.0F);

	{ // make icons
		IDigDatum* dig = nullptr;
		float icon_width, icon_height;
		double x, y;

		doc_dig->rewind();
		while ((dig = doc_dig->step()) != nullptr) {
			if (dig->type == DigDatumType::Icon) {
				IGraphlet* icon = dig->make_graphlet(&x, &y);

				if (icon != nullptr) {
					icon->fill_extent(0.0F, 0.0F, &icon_width, &icon_height);
					DigIconEntity^ entity = ref new DigIconEntity(icon, x, y, icon_width, icon_height);
					float2 ipos = icon_location(entity, map, initial_scale);
					
					this->planet->insert(icon, ipos.x, ipos.y, GraphletAnchor::LT);
					this->icons.push_back(entity);
				}
			}
		}
	}

	this->planet->end_update_sequence();

	this->graph_dig = doc_dig;
}

bool Diglet::ready() {
	return (this->graph_dig != nullptr);
}

void Diglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_width, h, this->view_height);
}

void Diglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	double time0 = current_inexact_milliseconds();
	Planetlet::draw(ds, x, y, Width, Height);

	ds->DrawRectangle(x, y, Width, Height,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		2.0F);
}

void Diglet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appdata_dig);

	draw_invalid_bitmap(hint, ds, x, y, Width, Height);
}

bool Diglet::on_key(VirtualKey key, bool screen_keyboard) {
	bool handled = false;

	if (this->map != nullptr) {
		this->planet->begin_update_sequence();

		handled = Planetlet::on_key(key, screen_keyboard);

		if (handled) {
			this->relocate_icons();
		}

		this->planet->end_update_sequence();
	}

	return handled;
}

bool Diglet::on_character(unsigned int keycode) {
	bool handled = false;
	
	if (this->map != nullptr) {
		this->planet->begin_update_sequence();

		handled = Planetlet::on_character(keycode);

		if (handled) {
			this->relocate_icons();
		}

		this->planet->end_update_sequence();
	}

	return handled;
}

void Diglet::relocate_icons() {
	DigMaplet* map = static_cast<DigMaplet*>(this->map);
	float new_scale = float(map->scale());

	this->planet->scale(new_scale);

	for (auto it = this->icons.begin(); it != this->icons.end(); it++) {
		DigIconEntity^ ent = static_cast<DigIconEntity^>((*it));
		float2 ipos = icon_location(ent, map, new_scale);

		this->planet->move_to(ent->icon, ipos.x, ipos.y, GraphletAnchor::LT);
	}

	this->notify_updated();
}
