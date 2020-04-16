#pragma once

#include <deque>

#include "graphlet/planetlet.hpp"
#include "graphlet/vessellet.hpp"

#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/filesystem/configuration/colorplotlet.hpp"
#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/xyzlet.hpp"
#include "graphlet/filesystem/project/tracelinelet.hpp"
#include "graphlet/filesystem/project/sectionlet.hpp"
#include "graphlet/filesystem/project/dredgetracklet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::DTPM {
	private enum class SailingMode { Free, VesselVisible, VesselCenter };

	private class Projectlet : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::DTPM::ProjectDocument, WarGrey::SCADA::Planetlet, WarGrey::DTPM::ProjectDoctype> {
	public:
		Projectlet(WarGrey::DTPM::IVessellet* vessel, WarGrey::DTPM::DredgeTracklet* track, WarGrey::DTPM::ColorPlotlet* plot,
			Platform::String^ project, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "projects");

	public:
		bool move_vessel(double x, double y, WarGrey::DTPM::SailingMode mode = SailingMode::VesselVisible);
		WarGrey::SCADA::double2& vessel_position();
		const WarGrey::DTPM::Outline* section(double geo_x, double geo_y);

	public:
		void center_vessel();
		void translate(float deltaX, float deltaY);
		void zoom(float zx, float zy, float deltaScale);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;
		bool on_wheel_translation(float x, float y, float delta, bool horizontal) override;
		bool on_wheel_zoom(float x, float y, float delta) override;

	public:
		void push_managed_map_objects(MapObject* mapobj);

	protected:
		WarGrey::DTPM::ProjectDoctype filter_file(Platform::String^ dirpath, Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_dig, WarGrey::DTPM::ProjectDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDoctype type) override {}

	private:
		void on_dig(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_dig);
		void on_xyz(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_xyz);
		void on_sec(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_sec);
		void on_traceline(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_dat);
		void on_map_logue(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_log);
		void on_depth_logue(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_log);
		void on_section_logue(Platform::String^ ms_appdata, WarGrey::DTPM::ProjectDocument^ doc_log);

	private:
		void insert_icons(WarGrey::DTPM::DigDoc^ doc_dig);
		bool relocate_vessel(Windows::Foundation::Numerics::float2* vpos = nullptr);
		void relocate_icons();

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		WarGrey::DTPM::DigMaplet* map;
		WarGrey::DTPM::IVessellet* vessel;
		WarGrey::DTPM::DredgeTracklet* track;
		WarGrey::DTPM::ColorPlotlet* plot;
		WarGrey::DTPM::Xyzlet* depth_xyz;
		WarGrey::DTPM::Tracelinelet* jobs_dat;
		WarGrey::DTPM::Sectionlet* front_sec;
		std::deque<Platform::Object^> icons;

	private: // the life cycle of map objects are not managed by Projectlet
		std::deque<MapObject*> managed_mapects;

	private:
		Windows::Foundation::Size view_size;
		WarGrey::SCADA::double2 vessel_pos;
	};
}
