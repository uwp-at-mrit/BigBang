#include "graphlet/filesystem/projectlet.hpp"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

#include "datum/flonum.hpp"
#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

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
Projectlet::Projectlet(Platform::String^ project, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new DigFrame(project), GraphletAnchor::LT, background), view_width(view_width), view_height(view_height), map(nullptr) {
	this->ms_appdata_rootdir = ((rootdir == nullptr) ? project : rootdir + "\\" + project);
	this->enable_stretch(false, false);
	this->enable_events(true, true);
}

Projectlet::~Projectlet() {
}

void Projectlet::construct() {
	Planetlet::construct();

	this->cd(this->ms_appdata_rootdir);
}

void Projectlet::on_dig_logue(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	DigLog^ dig_log = static_cast<DigLog^>(doc);

	for (size_t idx = 0; idx < dig_log->digs.size(); idx++) {
		if (dig_log->visibles[idx]) {
			Platform::String^ dig = dig_log->digs[idx];
			Platform::String^ ext = file_extension_from_path(dig);

			if (ext->Equals(".DIG")) {
				this->load_file(dig, ProjectDoctype::DIG);
			}
		}
	}
}

void Projectlet::on_dig(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	DigDoc^ doc_dig = static_cast<DigDoc^>(doc);
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

					{ // TODO: find out the reason why some icons do not like their locations?
					  // Nonetheless, icons will be relocated when the map is translated or scaled. 

						DigIconEntity^ entity = ref new DigIconEntity(icon, x, y, icon_width, icon_height);
						float2 ipos = icon_location(entity, map, initial_scale);

						this->planet->insert(icon, ipos.x, ipos.y, GraphletAnchor::LT);
						this->icons.push_back(entity);
					}
				}
			}
		}
	}

	this->planet->end_update_sequence();

	this->graph_dig = doc_dig;
}

bool Projectlet::ready() {
	return (this->graph_dig != nullptr);
}

void Projectlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_width, h, this->view_height);
}

void Projectlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	double time0 = current_inexact_milliseconds();
	Planetlet::draw(ds, x, y, Width, Height);

	ds->DrawRectangle(x, y, Width, Height,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		2.0F);
}

void Projectlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appdata_rootdir);

	draw_invalid_bitmap(hint, ds, x, y, Width, Height);
}

bool Projectlet::on_key(VirtualKey key, bool screen_keyboard) {
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

bool Projectlet::on_character(unsigned int keycode) {
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

void Projectlet::relocate_icons() {
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

ProjectDoctype Projectlet::filter_file(Platform::String^ filename, Platform::String^ _ext) {
	ProjectDoctype ft = ProjectDoctype::_;

	if (filename->Equals("Back.LOG")) {
		ft = ProjectDoctype::DIG_LOG;
	}

	return ft;
}

void Projectlet::on_appdata(Platform::String^ ms_appdata, ProjectDocument^ doc, ProjectDoctype type) {
	switch (type) {
	case ProjectDoctype::DIG_LOG: this->on_dig_logue(ms_appdata, doc); break;
	case ProjectDoctype::DIG: this->on_dig(ms_appdata, doc); break;
	}
}
