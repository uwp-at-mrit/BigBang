#pragma once

#include <deque>

#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/depthlet.hpp"

#include "graphlet/planetlet.hpp"
#include "graphlet/vessellet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private class Projectlet : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::ProjectDocument, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ProjectDoctype> {
	public:
		virtual ~Projectlet() noexcept;

		Projectlet(WarGrey::SCADA::IVessellet* vessel,
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
		void on_appdata(Platform::String^ file, WarGrey::SCADA::ProjectDocument^ doc_dig, WarGrey::SCADA::ProjectDoctype type) override;
		void on_appdata_not_found(Platform::String^ file, ProjectDoctype type) override {}

	private:
		void on_dig_logue(Platform::String^ file, WarGrey::SCADA::ProjectDocument^ doc_dig);
		void on_dig(Platform::String^ file, WarGrey::SCADA::ProjectDocument^ doc_dig);
		void on_xyz_logue(Platform::String^ file, WarGrey::SCADA::ProjectDocument^ doc_dig);
		void on_xyz(Platform::String^ file, WarGrey::SCADA::ProjectDocument^ doc_dig);

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::ProjectDocument^ graph_dig;

	private:
		Platform::String^ ms_appdata_rootdir;

	private: // graphlets are managed by the Planetlet
		WarGrey::SCADA::DigMaplet* map;
		WarGrey::SCADA::IVessellet* vessel;
		WarGrey::SCADA::Depthlet* depth;
		std::deque<Platform::Object^> icons;
		Windows::Foundation::Size view_size;

	private:
		double latitude;
		double longitude;
		double altitude;
		double vessel_x;
		double vessel_y;
	};
}
