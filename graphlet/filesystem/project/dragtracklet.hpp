#pragma once

#include <utility>

#include "graphlet/vessellet.hpp"
#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

namespace WarGrey::DTPM {
	private ref class DragTrack sealed {
	public:
		static WarGrey::DTPM::DragTrack^ load(Platform::String^ path);
		static bool save(WarGrey::DTPM::DragTrack^ self, Platform::String^ path);

	public:
		DragTrack(DragTrack^ src = nullptr);

	public:
		void refresh(DragTrack^ src);

	internal:
		unsigned long long begin_timepoint;
		unsigned long long end_timepoint;
		unsigned long long after_image_period;
		bool ps_visible;
		bool sb_visible;

	internal:
		double depth0;
		double interval;

	internal:
		float track_width;
		unsigned int track_color;
	};

	private class DragTracklet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::DTPM::DragTrack, WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~DragTracklet() noexcept;

		DragTracklet(WarGrey::DTPM::IVessellet* vessel, Platform::String^ profile, float width, float height = 0.0F,
			Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void update_outline(const WarGrey::DTPM::Outline* outline, double vessel_x, double vessel_y);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		WarGrey::DTPM::DragTrack^ clone_profile(WarGrey::DTPM::DragTrack^ dest = nullptr, bool real_profile = true);
		void preview(DragTrack^ src);
		void refresh(DragTrack^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ profile, WarGrey::DTPM::DragTrack^ profile_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ haxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ profile;

	private:
		WarGrey::DTPM::DragTrack^ preview_config;
		WarGrey::DTPM::DragTrack^ profile_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		WarGrey::DTPM::Outline* outline;
		double direction_sign;

	private:
		WarGrey::DTPM::IVessellet* vessel;
		double vessel_x;
		double vessel_y;

	private:
		float width;
		float height;
	};
}
