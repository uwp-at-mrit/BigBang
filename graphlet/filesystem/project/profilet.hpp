#pragma once

#include <utility>

#include "graphlet/vessellet.hpp"
#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

namespace WarGrey::DTPM {
	private ref class Profile sealed {
	public:
		static WarGrey::DTPM::Profile^ load(Platform::String^ path);
		static bool save(WarGrey::DTPM::Profile^ self, Platform::String^ path);

	public:
		Profile(Profile^ src = nullptr);

	public:
		void refresh(Profile^ src);

	internal:
		double width;
		double min_depth;
		double max_depth;

	internal:
		double depth_distance;
		double dragheads_distance;
	};

	private struct ProfileStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ profile_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ profile_style;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ ps_draghead_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sb_draghead_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ centerline_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ haxes_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ haxes_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ vaxes_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ vaxes_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;

		float profile_thickness = -1.0F;
		float centerline_thickness = -1.0F;
		float haxes_thickness = -1.0F;
		float vaxes_thickness = -1.0F;
		float border_thickness = -1.0F;

		int haxes_count = -1;
		int vaxes_half_count = -1;
	};

	WarGrey::DTPM::ProfileStyle default_profile_style(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ ps_color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sb_color = nullptr);

	private class Profilet 
		: public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::DTPM::Profile, WarGrey::SCADA::IGraphlet>
		, public virtual WarGrey::DTPM::IProfileRegion {
	public:
		virtual ~Profilet() noexcept;

		Profilet(WarGrey::DTPM::IVessellet* vessel, Platform::String^ profile, float width, float height = 0.0F,
			Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		
		Profilet(WarGrey::DTPM::ProfileStyle& style, WarGrey::DTPM::IVessellet* vessel, Platform::String^ profile,
			float width, float height = 0.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void update_outline(const WarGrey::DTPM::Outline* outline, double vessel_x, double vessel_y);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		Windows::Foundation::Numerics::float2 vessel_to_local(double x, double y, double depth) override;
		void fill_scale(double* xscale, double* yscale) override;

	public:
		WarGrey::DTPM::Profile^ clone_profile(WarGrey::DTPM::Profile^ dest = nullptr, bool real_profile = true);
		void preview(Profile^ src);
		void refresh(Profile^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ profile, WarGrey::DTPM::Profile^ profile_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		Windows::Foundation::Numerics::float2 distance_to_local(double distance, double depth, double xscale, double yscale);
		
	private:
		void update_horizontal_axes();
		void update_vertical_axes();
		void update_outline();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ haxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ profile;

	private:
		WarGrey::DTPM::ProfileStyle style;

	private:
		WarGrey::DTPM::Profile^ preview_config;
		WarGrey::DTPM::Profile^ profile_config;
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
