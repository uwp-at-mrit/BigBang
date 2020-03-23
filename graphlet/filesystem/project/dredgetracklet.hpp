#pragma once

#include <utility>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/configuration/colorplotlet.hpp"

#include "datum/flonum.hpp"

#include "brushes.hxx"

namespace WarGrey::DTPM {
	private enum class DredgeTrackType : unsigned int { // order matters
		GPS, PSDrag, SBDrag, Reamer,
		_
	};

	private ref class DredgeTrack sealed {
	public:
		static WarGrey::DTPM::DredgeTrack^ load(Platform::String^ path);
		static bool save(WarGrey::DTPM::DredgeTrack^ self, Platform::String^ path);

	public:
		DredgeTrack(DredgeTrack^ src = nullptr);

	public:
		void refresh(DredgeTrack^ src);
	
	internal:
		double depth0;
		double subinterval;
		double partition_distance;
		double after_image_period;

	internal:
		bool visibles[_N(DredgeTrackType)];

	internal:
		long long begin_timepoint;
		long long end_timepoint;
		bool show_history;

	internal:
		float track_width;
	};

	/************************************************************************************************/
	private class ITrackDataReceiver abstract {
	public:
		virtual void begin_maniplation_sequence(uint8 id) {}
		virtual void on_datum_values(uint8 id, long long open_s, long long timepoint_ms, long long type, WarGrey::SCADA::double3& dot) = 0;
		virtual void end_maniplation_sequence(uint8 id) {}

	public:
		virtual void on_maniplation_complete(uint8 id, long long open_s, long long close_s) {}
	};

	private class ITrackDataSource abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool ready() = 0;
		virtual bool loading() = 0;
		virtual void cancel() = 0;

	public:
		virtual void load(WarGrey::DTPM::ITrackDataReceiver* receiver, uint8 id, long long open_s, long long close_s) = 0;
		virtual void save(long long timepoint, long long type, WarGrey::SCADA::double3& dot) = 0;

	protected:
		~ITrackDataSource() noexcept {}
	};

	/************************************************************************************************/
	private class DredgeTracklet
		: public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::DTPM::DredgeTrack, WarGrey::SCADA::IGraphlet>
		, public virtual WarGrey::DTPM::ITrackDataReceiver {
	public:
		virtual ~DredgeTracklet() noexcept;

		DredgeTracklet(WarGrey::DTPM::ITrackDataSource* src,
			Platform::String^ track, float width, float height = 0.0F,
			Platform::String^ ext = ".config", Platform::String^ rootdir = "configuration");

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {}
		bool ready() override;

	public:
		WarGrey::DTPM::DredgeTrack^ clone_track(WarGrey::DTPM::DredgeTrack^ dest = nullptr, bool real_track = true);
		void preview(WarGrey::DTPM::DredgeTrack^ src);
		void refresh(WarGrey::DTPM::DredgeTrack^ src);

	public:
		void attach_to_map(WarGrey::DTPM::DigMaplet* master, bool force = false);
		void set_color_schema(WarGrey::DTPM::ColorPlotlet* plot, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback = WarGrey::SCADA::Colours::Chocolate, bool force = false);
		void filter_dredging_dot(WarGrey::DTPM::DredgeTrackType type, WarGrey::SCADA::double3& dot, bool persistent = true, long long timepoint_ms = 0LL);

	public:
		void on_datum_values(uint8 id, long long open_s, long long timepoint_ms, long long type, WarGrey::SCADA::double3& dot) override;
		void on_maniplation_complete(uint8 id, long long open_s, long long close_s) override;

	protected:
		void on_appdata(Windows::Foundation::Uri^ track, WarGrey::DTPM::DredgeTrack^ track_config) override;
		
	private:
		class Line;

	private:
		bool is_key_dot(WarGrey::SCADA::double3& dot, unsigned int id);
		void construct_line_if_necessary(unsigned int type);
		void clear_lines(WarGrey::DTPM::DredgeTracklet::Line** lines);
		void clear_key_dots();

	private:
		void draw_line(WarGrey::DTPM::DredgeTracklet::Line* line, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, long long start, long long end, double partition_squared,
			bool plot_ready, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback);

	private:
		WarGrey::DTPM::DredgeTrack^ preview_config;
		WarGrey::DTPM::DredgeTrack^ track_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		WarGrey::DTPM::ColorPlotlet* plot;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ fallback_color;

	private:
		WarGrey::DTPM::DigMaplet* master;
		WarGrey::DTPM::DredgeTracklet::Line* after_image_lines[_N(DredgeTrackType)];
		WarGrey::DTPM::DredgeTracklet::Line* realtime_lines[_N(DredgeTrackType)];
		WarGrey::DTPM::DredgeTracklet::Line* history_lines[_N(DredgeTrackType)];

	private:
		WarGrey::DTPM::ITrackDataSource* data_source;
		long long loading_history_timepoint;
		long long loading_after_image_timepoint;
		long long after_image_span;
		long long after_image_end;

	private:
		float width;
		float height;

	private:
		WarGrey::SCADA::double3 sentries[_N(DredgeTrackType)];
		double interval_squared;
	};
}
