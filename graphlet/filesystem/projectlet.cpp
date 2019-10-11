#include "graphlet/filesystem/projectlet.hpp"
#include "graphlet/filesystem/project/xyzdoc.hxx"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"

#include "datum/flonum.hpp"
#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

namespace {
	private class ProjectFrame : public Planet {
	public:
		virtual ~ProjectFrame() noexcept {}

		ProjectFrame(Platform::String^ name) : Planet(name) {}

	public:
		bool can_select(IGraphlet* g) override {
			return true;
		}

		bool can_select_multiple() override {
			return true;
		}

	public:
		void on_graphlet_ready(IGraphlet* g) override {
			this->get_logger()->log_message(Log::Info, "ProjectFrame");
		}

	public:
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& anchors, float x, float y) override {
			this->get_logger()->log_message(Log::Info, L"%d", anchors.size());
		}
	};

	private ref struct DigIconEntity sealed {
	internal:
		DigIconEntity(IGraphlet* icon, double x, double y, float width, float height)
			: icon(icon), x(x), y(y), size(Size(width, height)) {}

	internal:
		IGraphlet* icon;
		double x;
		double y;
		Size size;
	};

	static float2 graphlet_location(DigMaplet* map, double x, double y, Size& size, float scale) {
		float2 ipos = map->position_to_local(x, y);
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

		return float2(canvas_x - size.Width * 0.5F, canvas_y - size.Height);
	}
}

/*************************************************************************************************/
Projectlet::Projectlet(IVessellet* vessel, Platform::String^ project, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new ProjectFrame(project), GraphletAnchor::LT, background), view_size(Size(view_width, view_height)), vessel(vessel), map(nullptr) {
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

void Projectlet::on_xyz_logue(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	XyzLog^ xyz_log = static_cast<XyzLog^>(doc);

	for (size_t idx = 0; idx < xyz_log->xyzs.size(); idx++) {
		if (xyz_log->visibles[idx]) {
			this->load_file(xyz_log->xyzs[idx], ProjectDoctype::XYZ);
		}
	}
}

void Projectlet::on_dig(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	DigDoc^ doc_dig = static_cast<DigDoc^>(doc);
	DigMaplet* map = new DigMaplet(doc_dig, this->view_size.Width, this->view_size.Height);
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

					{ /** TODO
					   * Find out the reason why some icons do not like their locations?
					   * Nonetheless, icons will be relocated when the map is translated or scaled.
					   */

						DigIconEntity^ entity = ref new DigIconEntity(icon, x, y, icon_width, icon_height);
						float2 ipos = graphlet_location(map, x, y, entity->size, initial_scale);

						this->planet->insert(icon, ipos.x, ipos.y, GraphletAnchor::LT);
						this->icons.push_back(entity);
					}
				}
			}
		}
	}

	if (this->vessel != nullptr) {
		this->map->fill_anchor_position(0.5, 0.5, &this->vessel_x, &this->vessel_y);
		this->planet->insert(this->vessel, float(this->vessel_x), float(this->vessel_y), GraphletAnchor::CC);
	}

	this->planet->end_update_sequence();

	this->graph_dig = doc_dig;
}

void Projectlet::on_xyz(Platform::String^ ms_appdata, ProjectDocument^ doc) {

}

bool Projectlet::ready() {
	return (this->graph_dig != nullptr);
}

void Projectlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_size.Width, h, this->view_size.Height);
}

void Projectlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ location = make_wstring(L"X: %lf; Y: %lf.  B: %lf; L: %lf; H: %lf",
		this->vessel_x, this->vessel_y, this->latitude, this->longitude, this->altitude);

	Planetlet::draw(ds, x, y, Width, Height);

	ds->DrawText(location, x, y, Colours::GrayText, make_bold_text_format());
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
	if (this->map != nullptr) {
		float new_scale = float(this->map->scale());
		float2 ship_pos = graphlet_location(this->map, this->vessel_x, this->vessel_y, this->vessel->original_size(), new_scale);

		this->planet->move_to(this->vessel, ship_pos.x, ship_pos.y, GraphletAnchor::CC);
		
		this->planet->scale(new_scale);

		for (auto it = this->icons.begin(); it != this->icons.end(); it++) {
			DigIconEntity^ ent = static_cast<DigIconEntity^>((*it));
			float2 ipos = graphlet_location(this->map, ent->x, ent->y, ent->size, new_scale);

			this->planet->move_to(ent->icon, ipos.x, ipos.y, GraphletAnchor::LT);
		}

		this->notify_updated();
	}
}

void Projectlet::on_location_changed(double latitude, double longitude, double altitude, double x, double y) {
	this->latitude = latitude;
	this->longitude = longitude;
	this->altitude = altitude;
	this->vessel_x = x;
	this->vessel_y = y;

	this->relocate_icons();
}

/*************************************************************************************************/
ProjectDoctype Projectlet::filter_file(Platform::String^ filename, Platform::String^ _ext) {
	ProjectDoctype ft = ProjectDoctype::_;

	if (filename->Equals("Back.LOG")) {
		ft = ProjectDoctype::DIG_LOG;
	} else if (filename->Equals("Deep.LOG")) {
		ft = ProjectDoctype::XYZ_LOG;
	}

	return ft;
}

void Projectlet::on_appdata(Platform::String^ ms_appdata, ProjectDocument^ doc, ProjectDoctype type) {
	switch (type) {
	case ProjectDoctype::DIG_LOG: this->on_dig_logue(ms_appdata, doc); break;
	case ProjectDoctype::DIG: this->on_dig(ms_appdata, doc); break;
	case ProjectDoctype::XYZ_LOG: this->on_xyz_logue(ms_appdata, doc); break;
	case ProjectDoctype::XYZ: this->on_xyz(ms_appdata, doc); break;
	}
}
