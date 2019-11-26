#include "graphlet/filesystem/projectlet.hpp"

#include "graphlet/filesystem/project/reader/maplog.hxx"
#include "graphlet/filesystem/project/reader/depthlog.hxx"
#include "graphlet/filesystem/project/reader/sectionlog.hxx"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "math.hpp"
#include "planet.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private class ProjectFrame : public Planet {
	public:
		virtual ~ProjectFrame() noexcept {}

		ProjectFrame(Platform::String^ name) : Planet(name) {}

	public:
		bool can_select(IGraphlet* g) override {
			return false;
		}
	};

	private ref struct DigIconEntity sealed {
	internal:
		DigIconEntity(IGraphlet* icon, double x, double y) : icon(icon), x(x), y(y) {}

	internal:
		IGraphlet* icon;
		double x;
		double y;
	};

	private enum MapMode { Dredger = 0b01, ENChart = 0b10 };
}

/*************************************************************************************************/
Projectlet::Projectlet(IVessellet* vessel, ColorPlotlet* plot
	, Platform::String^ project, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Projectlet(vessel, plot, nullptr, project, view_width, view_height, background, rootdir) {}
	
Projectlet::Projectlet(IVessellet* vessel, ColorPlotlet* plot, S63let* enchart
	, Platform::String^ project, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new ProjectFrame(project), GraphletAnchor::LT, background)
	, view_size(Size(view_width, view_height)), vessel(vessel), plot(plot), map(nullptr), enchart(enchart) {
	this->ms_appdata_rootdir = ((rootdir == nullptr) ? project : rootdir + "\\" + project);
	this->enable_stretch(false, false);
	this->enable_events(true, true); // enable low level events for traceline.
	this->disable_wheel_translation(true);
}

void Projectlet::construct() {
	Planetlet::construct();

	if (this->enchart != nullptr) {
		ProjectFrame* frame = dynamic_cast<ProjectFrame*>(this->planet);

		frame->change_mode(ENChart);
		this->planet->insert(this->enchart);
		frame->change_mode(Dredger);
	}

	this->font = make_bold_text_format(32.0F);
	this->cd(this->ms_appdata_rootdir);
}

void Projectlet::on_map_logue(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	MapLog^ dig_log = static_cast<MapLog^>(doc);

	for (size_t idx = 0; idx < dig_log->maps.size(); idx++) {
		if (dig_log->visibles[idx]) {
			Platform::String^ dig = dig_log->maps[idx];
			Platform::String^ ext = file_extension_from_path(dig);

			if (ext->Equals(".DIG")) {
				this->load_file(dig, ProjectDoctype::DIG);
			}
		}
	}
}

void Projectlet::on_depth_logue(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	DepthLog^ depth_log = static_cast<DepthLog^>(doc);

	for (size_t idx = 0; idx < depth_log->depths.size(); idx++) {
		if (depth_log->visibles[idx]) {
			Platform::String^ depth = depth_log->depths[idx];
			Platform::String^ ext = file_extension_from_path(depth);

			if (ext->Equals(".XYZ")) {
				this->load_file(depth, ProjectDoctype::XYZ);
			}
		}
	}
}

void Projectlet::on_section_logue(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	SectionLog^ section_log = static_cast<SectionLog^>(doc);

	for (size_t idx = 0; idx < section_log->sections.size(); idx++) {
		Platform::String^ section = section_log->sections[idx];
		Platform::String^ ext = file_extension_from_path(section);

		if (ext->Equals(".SEC")) {
			this->load_file(section, ProjectDoctype::SEC);
		}
	}
}

void Projectlet::on_dig(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	DigDoc^ doc_dig = static_cast<DigDoc^>(doc);
	ProjectFrame* frame = dynamic_cast<ProjectFrame*>(this->planet);

	this->planet->begin_update_sequence();
	
	/** NOTE
	 * For the sake of simplicity, non-icon items are organized as a batch.
	 * Also, they are drawn before drawing icons.
	 *
	 * The modifyDIG draw icons firstly.
	 */
	
	frame->change_mode(Dredger);
	this->map = this->planet->insert_one(new DigMaplet(doc_dig, this->view_size.Width, this->view_size.Height));
	
	{ // make icons
		IDigDatum* dig = nullptr;
		double x, y;

		doc_dig->rewind();
		while ((dig = doc_dig->step()) != nullptr) {
			if (dig->type == DigDatumType::Icon) {
				IGraphlet* icon = dig->make_graphlet(&x, &y);

				if (icon != nullptr) {
					icon->enable_resizing(false);

					{ /** TODO
					   * Find out the reason why some icons do not like their locations?
					   * Nonetheless, icons will be relocated when the map is translated or scaled.
					   */

						DigIconEntity^ entity = ref new DigIconEntity(icon, x, y);
						float2 ipos = this->map->position_to_local(x, y);

						this->planet->insert(icon, ipos.x, ipos.y, GraphletAnchor::CC);
						this->icons.push_back(entity);
					}
				}
			}
		}
	}

	frame->change_mode(Dredger | ENChart);

	if (this->vessel != nullptr) {
		this->planet->insert(this->vessel, this->view_size.Width * 0.5F, this->view_size.Height * 0.5F, GraphletAnchor::CC);
	}

	if (this->depth_xyz != nullptr) {
		this->planet->insert(this->depth_xyz, 0.0F, 0.0F);
		this->depth_xyz->attach_to_map(this->map);
	}

	if (this->jobs_dat != nullptr) {
		this->planet->insert(this->jobs_dat, 0.0F, 0.0F);
		this->jobs_dat->attach_to_map(this->map);
	}

	if (this->front_sec != nullptr) {
		this->planet->insert(this->front_sec, 0.0F, 0.0F);
		this->front_sec->attach_to_map(this->map);
	}

	frame->change_mode(Dredger);

	this->planet->end_update_sequence();

	this->graph_dig = doc_dig;
}

void Projectlet::on_xyz(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	XyzDoc^ doc_xyz = static_cast<XyzDoc^>(doc);

	this->depth_xyz = this->planet->insert_one(new Xyzlet(doc_xyz));
	this->depth_xyz->set_color_schema(this->plot);
	
	if (this->map != nullptr) {
		ProjectFrame* frame = dynamic_cast<ProjectFrame*>(this->planet);

		frame->change_mode(Dredger | ENChart);
		this->planet->begin_update_sequence();
		this->planet->insert(this->depth_xyz, 0.0F, 0.0F);
		this->depth_xyz->set_color_schema(this->plot);
		this->depth_xyz->attach_to_map(this->map);
		this->planet->end_update_sequence();
		frame->change_mode(Dredger);
	}
}

void Projectlet::on_traceline(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	JobDoc^ xh_dat = static_cast<JobDoc^>(doc);
	
	this->jobs_dat = new Tracelinelet(xh_dat);

	if (this->map != nullptr) {
		ProjectFrame* frame = dynamic_cast<ProjectFrame*>(this->planet);

		frame->change_mode(Dredger | ENChart);
		this->planet->begin_update_sequence();
		this->planet->insert(this->jobs_dat, 0.0F, 0.0F);
		this->jobs_dat->attach_to_map(this->map);
		this->planet->end_update_sequence();
		frame->change_mode(Dredger);
	}
}

void Projectlet::on_sec(Platform::String^ ms_appdata, ProjectDocument^ doc) {
	SecDoc^ doc_sec = static_cast<SecDoc^>(doc);

	this->front_sec = new FrontalSectionlet(doc_sec, true);

	if (this->map != nullptr) {
		ProjectFrame* frame = dynamic_cast<ProjectFrame*>(this->planet);

		frame->change_mode(Dredger | ENChart);
		this->planet->begin_update_sequence();
		this->planet->insert(this->front_sec, 0.0F, 0.0F);
		this->front_sec->attach_to_map(this->map);
		this->planet->end_update_sequence();
		frame->change_mode(Dredger);
	}
}

bool Projectlet::ready() {
	return (this->graph_dig != nullptr);
}

void Projectlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_size.Width, h, this->view_size.Height);
}

void Projectlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float border_thickness = 2.0F;
	float offset = border_thickness * 0.5F;

	Planetlet::draw(ds, x, y, Width, Height);

	ds->DrawRectangle(x + offset, y + offset, Width - border_thickness, Height - border_thickness,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		border_thickness);
}

void Projectlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TextExtent te;
	CanvasGeometry^ pname = paragraph(file_name_from_path(this->ms_appdata_rootdir), this->font, &te);
	float px = x + (Width - te.width) * 0.5F;
	float py = y + (Height - te.height) * 0.5F;

	this->draw(ds, x, y, Width, Height);
	ds->FillGeometry(pname, px, py, Colours::GrayText);
}

void Projectlet::center_vessel() {
	if (this->map != nullptr) {
		this->planet->begin_update_sequence();
		this->map->center_at(this->vessel_x, this->vessel_y);
		this->relocate_vessel();
		this->relocate_icons();
		this->planet->end_update_sequence();
	}
}

void Projectlet::translate(float deltaX, float deltaY) {
	if (this->map != nullptr) {
		this->planet->begin_update_sequence();
		this->map->translate(deltaX, deltaY);
		this->relocate_vessel();
		this->relocate_icons();
		this->planet->end_update_sequence();
	}
}

void Projectlet::zoom(float zx, float zy, float deltaScale) {
	if (this->map != nullptr) {
		this->planet->begin_update_sequence();
		this->map->zoom(zx, zy, deltaScale);
		this->relocate_vessel();
		this->relocate_icons();
		this->planet->end_update_sequence();
	}
}

bool Projectlet::on_key(VirtualKey key, bool screen_keyboard) {
	bool handled = false;

	if (this->map != nullptr) {
		this->planet->begin_update_sequence();

		handled = Planetlet::on_key(key, screen_keyboard);

		if (!handled) {
			handled = true;

			switch (key) {
			case VirtualKey::Left: this->map->transform(MapMove::Left); break;
			case VirtualKey::Right: this->map->transform(MapMove::Right); break;
			case VirtualKey::Up: this->map->transform(MapMove::Up); break;
			case VirtualKey::Down: this->map->transform(MapMove::Down); break;
			case VirtualKey::H: this->map->center_at(this->vessel_x, this->vessel_y); break;
			default: handled = false;
			}
		}

		if (handled) {
			this->relocate_vessel();
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

		if (!handled) {
			handled = true;

			switch (keycode) {
			case 61 /* = */: case 43 /* + */: this->map->transform(MapMove::ZoomIn); break;
			case 45 /* - */: case 95 /* _ */: this->map->transform(MapMove::ZoomOut); break;
			case 8 /* back */: this->map->transform(MapMove::Reset); break;
			default: handled = false;
			}
		}

		if (handled) {
			this->relocate_vessel();
			this->relocate_icons();
		}

		this->planet->end_update_sequence();
	}

	return handled;
}

bool Projectlet::on_wheel_translation(float x, float y, float delta, bool horizontal) {
	bool handled = (this->map != nullptr);

	if (handled) {
		this->planet->begin_update_sequence();

		if (horizontal) {
			this->translate(delta, 0.0F);
		} else {
			this->translate(0.0F, delta);
		}

		if (handled) {
			this->relocate_vessel();
			this->relocate_icons();
		}

		this->planet->end_update_sequence();
	}

	return handled;
}

bool Projectlet::on_wheel_zoom(float x, float y, float delta) {
	bool handled = (this->map != nullptr);
	
	if (handled) {
		this->planet->begin_update_sequence();

		this->zoom(x, y, delta);

		if (handled) {
			this->relocate_vessel();
			this->relocate_icons();
		}

		this->planet->end_update_sequence();
	}

	return handled;
}

bool Projectlet::relocate_vessel(float2* local_pos) {
	bool moved = false;

	if (this->vessel != nullptr) {
		float2 vpos = this->map->position_to_local(this->vessel_x, this->vessel_y);

		this->vessel->scale(this->map->actual_scale());
		this->planet->move_to(this->vessel, vpos.x, vpos.y, GraphletAnchor::CC);

		SET_BOX(local_pos, vpos);

		moved = true;
	}

	return moved;
}

void Projectlet::relocate_icons() {
	for (auto it = this->icons.begin(); it != this->icons.end(); it++) {
		DigIconEntity^ ent = static_cast<DigIconEntity^>(*it);
		float2 ipos = this->map->position_to_local(ent->x, ent->y);

		this->planet->move_to(ent->icon, ipos.x, ipos.y, GraphletAnchor::CC);
	}
}

bool Projectlet::move_vessel(double x, double y, SailingMode mode) {
	bool moved = false;

	if ((this->vessel_x != x) || (this->vessel_y != y)) {
		float2 vpos;
		this->vessel_x = x;
		this->vessel_y = y;

		if (this->map != nullptr) {
			this->planet->begin_update_sequence();

			if (this->relocate_vessel(&vpos)) {
				moved = true;

				if (this->jobs_dat != nullptr) {
					this->jobs_dat->on_vessel_move(this->vessel_x, this->vessel_y);
				}

				switch (mode) {
				case SailingMode::VesselCenter: this->center_vessel(); break;
				case SailingMode::VesselVisible: {
					float vwidth, vheight;

					this->vessel->fill_extent(vpos.x, vpos.y, &vwidth, &vheight);

					if (!rectangle_contain(vwidth, vheight, this->view_size.Width - vwidth, this->view_size.Height - vheight, vpos)) {
						this->center_vessel();
					}
				}; break;
				}
			}

			this->planet->end_update_sequence();
		}
	}

	return moved;
}

const TransversePlane* Projectlet::section(double x, double y) {
	const TransversePlane* plane = nullptr;

	if (this->front_sec != nullptr) {
		plane = this->front_sec->section(x, y);
	}

	return plane;
}

/*************************************************************************************************/
ProjectDoctype Projectlet::filter_file(Platform::String^ filename, Platform::String^ _ext) {
	ProjectDoctype ft = ProjectDoctype::_;

	if (filename->Equals("Back.LOG")) {
		ft = ProjectDoctype::Map_LOG;
	} else if (filename->Equals("Deep.LOG")) {
		ft = ProjectDoctype::Depth_LOG;
	} else if (filename->Equals("Section.LOG")) {
		ft = ProjectDoctype::Section_LOG;
	} else if (filename->Equals("XH.DAT")) {
		ft = ProjectDoctype::Traceline;
	}

	return ft;
}

void Projectlet::on_appdata(Platform::String^ ms_appdata, ProjectDocument^ doc, ProjectDoctype type) {
	switch (type) {
	case ProjectDoctype::DIG:         this->on_dig(ms_appdata, doc); break;
	case ProjectDoctype::XYZ:         this->on_xyz(ms_appdata, doc); break;
	case ProjectDoctype::SEC:         this->on_sec(ms_appdata, doc); break;
	case ProjectDoctype::Traceline:   this->on_traceline(ms_appdata, doc); break;
	case ProjectDoctype::Map_LOG:     this->on_map_logue(ms_appdata, doc); break;
	case ProjectDoctype::Depth_LOG:   this->on_depth_logue(ms_appdata, doc); break;
	case ProjectDoctype::Section_LOG: this->on_section_logue(ms_appdata, doc); break;
	}
}
