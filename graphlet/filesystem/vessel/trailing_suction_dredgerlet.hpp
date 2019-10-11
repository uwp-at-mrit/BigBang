#pragma once

#include <deque>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/msappdataloguelet.hxx"

#include "graphlet/vessellet.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private ref class TrailingSuctionDredger sealed : public WarGrey::SCADA::IVessel {
	public:
		static WarGrey::SCADA::TrailingSuctionDredger^ load(Platform::String^ path);
		static bool save(WarGrey::SCADA::TrailingSuctionDredger^ self, Platform::String^ path);

	public:
		TrailingSuctionDredger(TrailingSuctionDredger^ src = nullptr);
		
	public:
		void refresh(TrailingSuctionDredger^ src);

	internal:
		WarGrey::SCADA::double2 gps[2];
		WarGrey::SCADA::double2 ps_suction;
		WarGrey::SCADA::double2 sb_suction;
		WarGrey::SCADA::double2 hopper_vertexes[4];
		WarGrey::SCADA::double2 body_vertexes[7];
		WarGrey::SCADA::double2 bridge_vertexes[10];
		WarGrey::SCADA::double2 trunnion;
		WarGrey::SCADA::double2 barge;
	};

	private class TrailingSuctionDredgerlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::TrailingSuctionDredger, WarGrey::SCADA::IVessellet> {
	public:
		virtual ~TrailingSuctionDredgerlet() noexcept;

		TrailingSuctionDredgerlet(Platform::String^ vessel, float scale = 1.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void resize(float width, float height) override;
		bool ready() override;

	public:
		Windows::Foundation::Size original_size() override;

	public:
		WarGrey::SCADA::TrailingSuctionDredger^ clone_vessel(WarGrey::SCADA::TrailingSuctionDredger^ dest = nullptr);
		void preview(TrailingSuctionDredger^ src);
		void refresh(TrailingSuctionDredger^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ vessel, WarGrey::SCADA::TrailingSuctionDredger^ vessel_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		void reconstruct(Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hopper;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bridge;

	private:
		WarGrey::SCADA::TrailingSuctionDredger^ vessel_config;
		WarGrey::SCADA::TrailingSuctionDredger^ preview_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		float original_scale;
		float xscale;
		float yscale;
		float xradius;
		float yradius;
	};
}
