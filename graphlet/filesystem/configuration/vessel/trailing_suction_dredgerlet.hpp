#pragma once

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/device/draglet.hpp"
#include "graphlet/vessellet.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private ref class TrailingSuctionDredger sealed : public WarGrey::DTPM::IVesselShape {
	public:
		static WarGrey::DTPM::TrailingSuctionDredger^ load(Platform::String^ path);
		static bool save(WarGrey::DTPM::TrailingSuctionDredger^ self, Platform::String^ path);

	public:
		TrailingSuctionDredger(TrailingSuctionDredger^ src = nullptr);
		
	public:
		void refresh(TrailingSuctionDredger^ src);

	internal:
		WarGrey::SCADA::double2 gps[2];
		WarGrey::SCADA::double2 ps_suction;
		WarGrey::SCADA::double2 sb_suction;
		WarGrey::SCADA::double2 hopper_vertices[4];
		WarGrey::SCADA::double2 body_vertices[7];
		WarGrey::SCADA::double2 bridge_vertices[10];
		WarGrey::SCADA::double2 trunnion;
		WarGrey::SCADA::double2 barge;
	};

	private struct TrailingSuctionDredgerStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bridge_border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bridge_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hopper_border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hopper_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ps_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ sb_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ gps_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ offset_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ barge_color;

		float gps_radius = -1.0;
		float barge_radius = -1.0;
	};

	WarGrey::DTPM::TrailingSuctionDredgerStyle default_trailing_suction_dredger_style(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bridge_border_color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hopper_border_color = nullptr);

	private class TrailingSuctionDredgerlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::DTPM::TrailingSuctionDredger, WarGrey::DTPM::IVessellet> {
	public:
		virtual ~TrailingSuctionDredgerlet() noexcept;

		TrailingSuctionDredgerlet(Platform::String^ vessel, float scale = 1.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		TrailingSuctionDredgerlet(Platform::String^ vessel, WarGrey::DTPM::TrailingSuctionDredgerStyle& style,
			float scale = 1.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		void draw_profile(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, WarGrey::DTPM::IProfileRegion* profilet, float cx, float y, float half_width, float height) override;
		void resize(float width, float height) override;
		bool ready() override;

	public:
		void set_bow_direction(double degrees) override;
		Windows::Foundation::Size original_size() override;

	public:
		void set_ps_drag_info(WarGrey::SCADA::DragInfo& info, unsigned int actual_drag_pipe_size = 2U);
		void set_sb_drag_info(WarGrey::SCADA::DragInfo& info, unsigned int actual_drag_pipe_size = 2U);
		void set_ps_drag_figures(WarGrey::SCADA::double3& offset, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead);
		void set_sb_drag_figures(WarGrey::SCADA::double3& offset, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead);
		
		void fill_ps_track_position(WarGrey::SCADA::double3* pos, WarGrey::SCADA::double2& self_pos);
		void fill_sb_track_position(WarGrey::SCADA::double3* pos, WarGrey::SCADA::double2& self_pos);

	public:
		WarGrey::DTPM::TrailingSuctionDredger^ clone_vessel(WarGrey::DTPM::TrailingSuctionDredger^ dest = nullptr, bool real_vessel = true);
		void preview(TrailingSuctionDredger^ src);
		void refresh(TrailingSuctionDredger^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ vessel, WarGrey::DTPM::TrailingSuctionDredger^ vessel_config) override;
		//void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		void reconstruct();
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hopper;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bridge;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ps_drag_head;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ sb_drag_head;
		WarGrey::DTPM::TrailingSuctionDredgerStyle style;

	private:
		WarGrey::DTPM::TrailingSuctionDredger^ vessel_config;
		WarGrey::DTPM::TrailingSuctionDredger^ preview_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		WarGrey::SCADA::double2 current_gps;
		double bow_direction;
		float original_scale;
		float xscale;
		float yscale;

	private:
		Windows::Foundation::Numerics::float2 gps[2];
		Windows::Foundation::Numerics::float2 ps_suction;
		Windows::Foundation::Numerics::float2 sb_suction;
		Windows::Foundation::Numerics::float2 ps_offset;
		Windows::Foundation::Numerics::float2 ps_ujoints[DRAG_SEGMENT_MAX_COUNT];
		Windows::Foundation::Numerics::float2 ps_draghead;
		Windows::Foundation::Numerics::float2 sb_offset;
		Windows::Foundation::Numerics::float2 sb_ujoints[DRAG_SEGMENT_MAX_COUNT];
		Windows::Foundation::Numerics::float2 sb_draghead;
		Windows::Foundation::Numerics::float2 trunnion;
		Windows::Foundation::Numerics::float2 barge;

	private:
		struct Drag;
		WarGrey::DTPM::TrailingSuctionDredgerlet::Drag* ps_drag;
		WarGrey::DTPM::TrailingSuctionDredgerlet::Drag* sb_drag;
	};
}
