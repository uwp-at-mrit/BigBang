#pragma once

#include <deque>

#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/filesystem/configuration/colorplotlet.hpp"
#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/xyzlet.hpp"

#include "graphlet/planetlet.hpp"
#include "graphlet/vessellet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private class Projectlet : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::ProjectDocument, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ProjectDoctype> {
	public:
		Projectlet(WarGrey::SCADA::IVessellet* vessel, WarGrey::SCADA::ColorPlotlet* plot,
			Platform::String^ project, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "projects");

	public:
		void on_location_changed(double latitude, double longitude, double altitude, double x, double y);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;

	protected:
		WarGrey::SCADA::ProjectDoctype filter_file(Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig, WarGrey::SCADA::ProjectDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, ProjectDoctype type) override {}

	private:
		void on_dig(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig);
		void on_xyz(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_xyz);
		void on_traceline(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dat);
		void on_map_logue(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_log);
		void on_depth_logue(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_log);

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::ProjectDocument^ graph_dig;

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		WarGrey::SCADA::DigMaplet* map;
		WarGrey::SCADA::IVessellet* vessel;
		WarGrey::SCADA::ColorPlotlet* plot;
		WarGrey::SCADA::Xyzlet* depth_xyz;
		std::deque<Platform::Object^> icons;

	private:
		Windows::Foundation::Size view_size;
		double latitude;
		double longitude;
		double altitude;
		double vessel_x;
		double vessel_y;
	};
}
