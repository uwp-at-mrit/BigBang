#pragma once

#include <utility>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private class FrontalSectionlet : public WarGrey::SCADA::IGraphlet {
	public:
		FrontalSectionlet(WarGrey::SCADA::SecDoc^ sec, bool draw_slope_lines = false, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		void attach_to_map(WarGrey::SCADA::DigMaplet* master, bool force = false);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ slope_style;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ centerline_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ sideline_color;

	private:
		WarGrey::SCADA::DigMaplet* master;
		WarGrey::SCADA::SecDoc^ doc_sec;
		std::deque<std::deque<std::pair<WarGrey::SCADA::double3, WarGrey::SCADA::double3>>> slope_segments;

	private:
		float thickness;
		bool draw_slope_lines;
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

	private class TransverseSectionlet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::TransverseSection, WarGrey::SCADA::IGraphlet> {
	public:
		virtual ~TransverseSectionlet() noexcept;
		TransverseSectionlet(Platform::String^ section, float width, float height = 0.0F, Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

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
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private:
		WarGrey::SCADA::TransverseSection^ preview_config;
		WarGrey::SCADA::TransverseSection^ section_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		float width;
		float height;
	};
}
