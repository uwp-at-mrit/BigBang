#pragma once

#include <utility>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private class FrontalSectionlet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		virtual ~FrontalSectionlet() noexcept;
		FrontalSectionlet(WarGrey::SCADA::SecDoc^ sec, bool draw_slope_lines = false, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ section_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);
		void section(double x, double y);

	private:
		void section(double x, double y, double center_x, double center_y);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ slope_style;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ section_color;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::SecDoc^ doc_sec;
		std::deque<std::deque<std::pair<WarGrey::SCADA::double3, WarGrey::SCADA::double3>>> slope_segments;

	private:
		float thickness;
		bool draw_slope_lines;

	private:
		WarGrey::SCADA::double3 centerfoot;
		WarGrey::SCADA::double3* intersections;
		WarGrey::SCADA::double3* interslopes;
		WarGrey::SCADA::double2 ps_boundry;
		WarGrey::SCADA::double2 sb_boundry;
		int intersection_count;
		int interslope_count;
	};

	/************************************************************************************************/
	private ref class TransverseSection sealed {
	public:
		static WarGrey::SCADA::TransverseSection^ load(Platform::String^ path);
		static bool save(WarGrey::SCADA::TransverseSection^ self, Platform::String^ path);

	public:
		TransverseSection(TransverseSection^ src = nullptr);

	public:
		void refresh(TransverseSection^ src);

	internal:
		double width;
		double min_depth;
		double max_depth;

	internal:
		double depth_distance;
		double dragheads_distance;
	};

	private struct TransverseSectionStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ ps_draghead_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sb_draghead_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ centerline_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ haxes_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ haxes_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ vaxes_color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ vaxes_style;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;

		float centerline_thickness = -1.0F;
		float haxes_thickness = -1.0F;
		float vaxes_thickness = -1.0F;
		float border_thickness = -1.0F;

		int haxes_count = -1;
		int vaxes_half_count = -1;
	};

	WarGrey::SCADA::TransverseSectionStyle default_transverse_section_style(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ ps_color = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sb_color = nullptr);

	private class TransverseSectionlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::TransverseSection, WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~TransverseSectionlet() noexcept;
		TransverseSectionlet(Platform::String^ section, float width, float height = 0.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");
		TransverseSectionlet(WarGrey::SCADA::TransverseSectionStyle& style, Platform::String^ section, float width, float height = 0.0F,
			Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		WarGrey::SCADA::TransverseSection^ clone_section(WarGrey::SCADA::TransverseSection^ dest = nullptr, bool real_section = true);
		void preview(TransverseSection^ src);
		void refresh(TransverseSection^ src);

	protected:
		void on_appdata(Windows::Foundation::Uri^ section, WarGrey::SCADA::TransverseSection^ section_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		void update_horizontal_axes();
		void update_vertical_axes();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ haxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;

	private:
		WarGrey::SCADA::TransverseSectionStyle style;

	private:
		WarGrey::SCADA::TransverseSection^ preview_config;
		WarGrey::SCADA::TransverseSection^ section_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		double centerline_position;

	private:
		float width;
		float height;
	};
}
