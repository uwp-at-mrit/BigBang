#pragma once

#include <deque>

#include "graphlet/planetlet.hpp"
#include "graphlet/vessellet.hpp"

#include "graphlet/filesystem/s63let.hpp"
#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/filesystem/configuration/colorplotlet.hpp"
#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/xyzlet.hpp"
#include "graphlet/filesystem/project/tracelinelet.hpp"
#include "graphlet/filesystem/project/sectionlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private enum class SailingMode { Free, VesselVisible, VesselCenter };

	private class Projectlet : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::ProjectDocument, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ProjectDoctype> {
	public:
		Projectlet(WarGrey::SCADA::IVessellet* vessel, WarGrey::SCADA::ColorPlotlet* plot,
			Platform::String^ project, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "projects");

		Projectlet(WarGrey::SCADA::IVessellet* vessel, WarGrey::SCADA::ColorPlotlet* plot, WarGrey::SCADA::S63let* enchart,
			Platform::String^ project, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "projects");

	public:
		bool move_vessel(double x, double y, WarGrey::SCADA::SailingMode mode = SailingMode::VesselVisible);
		const WarGrey::SCADA::TransversePlane* section(double x, double y);

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

	protected:
		WarGrey::SCADA::ProjectDoctype filter_file(Platform::String^ dirpath, Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig, WarGrey::SCADA::ProjectDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, ProjectDoctype type) override {}

	private:
		void on_dig(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig);
		void on_xyz(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_xyz);
		void on_sec(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_sec);
		void on_traceline(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dat);
		void on_map_logue(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_log);
		void on_depth_logue(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_log);
		void on_section_logue(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_log);

	private:
		bool relocate_vessel(Windows::Foundation::Numerics::float2* vpos = nullptr);
		void relocate_icons();

	private:
		WarGrey::SCADA::ProjectDocument^ graph_dig;

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		WarGrey::SCADA::DigMaplet* map;
		WarGrey::SCADA::IVessellet* vessel;
		WarGrey::SCADA::S63let* enchart;
		WarGrey::SCADA::ColorPlotlet* plot;
		WarGrey::SCADA::Xyzlet* depth_xyz;
		WarGrey::SCADA::Tracelinelet* jobs_dat;
		WarGrey::SCADA::FrontalSectionlet* front_sec;
		std::deque<Platform::Object^> icons;

	private:
		Windows::Foundation::Size view_size;
		double vessel_x;
		double vessel_y;
	};
}
