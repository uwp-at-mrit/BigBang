#pragma once

#include <deque>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/msappdataloguelet.hxx"

#include "graphlet/vessellet.hpp"

namespace WarGrey::SCADA {
	private ref struct TrailingSuctionDredger abstract {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::TrailingSuctionDredger^>^ load_async(Platform::String^ path);
		
	internal:
		Windows::Foundation::Numerics::float2 gps[2];
		Windows::Foundation::Numerics::float2 ps_suction;
		Windows::Foundation::Numerics::float2 sb_suction;
		Windows::Foundation::Numerics::float2 hopper_vertexes[4];
		Windows::Foundation::Numerics::float2 body_vertexes[7];
		Windows::Foundation::Numerics::float2 bridge_vertexes[10];
		Windows::Foundation::Numerics::float2 trunnion;
		Windows::Foundation::Numerics::float2 barge;
	};

	private class TrailingSuctionDredgerlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::TrailingSuctionDredger, WarGrey::SCADA::IVessellet, int> {
	public:
		virtual ~TrailingSuctionDredgerlet() noexcept;

		TrailingSuctionDredgerlet(Platform::String^ vessel, float real_width, float real_height,
			Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	protected:
		void on_appdata(Windows::Foundation::Uri^ vessel, WarGrey::SCADA::TrailingSuctionDredger^ vessel_config, int hint) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file, int hint) override {}

	private:
		WarGrey::SCADA::TrailingSuctionDredger^ vessel_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		float real_width;
		float real_height;
	};
}
