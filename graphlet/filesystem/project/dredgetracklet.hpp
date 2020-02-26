#pragma once

#include <utility>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"

#include "datum/flonum.hpp"

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
		unsigned long long begin_timepoint;
		unsigned long long end_timepoint;
		bool visibles[_N(DredgeTrackType)];

	internal:
		float track_width;
		unsigned int track_color;
	};

	/************************************************************************************************/
	private class ITrackDataReceiver abstract {
	public:
		virtual void begin_maniplation_sequence() {}
		virtual void on_datum_values(long long open_s, long long timepoint_ms, long long type, WarGrey::SCADA::double3& dot) = 0;
		virtual void end_maniplation_sequence() {}

	public:
		virtual void on_maniplation_complete(long long open_s, long long close_s) {}
	};

	private class ITrackDataSource abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool ready() = 0;
		virtual bool loading() = 0;
		virtual void cancel() = 0;

	public:
		virtual void load(WarGrey::DTPM::ITrackDataReceiver* receiver, long long open_s, long long close_s) = 0;
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
		void preview(DredgeTrack^ src);
		void refresh(DredgeTrack^ src);

	public:
		void attach_to_map(WarGrey::DTPM::DigMaplet* master, bool force = false);
		void push_track_dot(WarGrey::DTPM::DredgeTrackType type, WarGrey::SCADA::double3& dot, bool persistent = true, long long timepoint_ms = 0LL);

	public:
		void on_datum_values(long long open_s, long long timepoint_ms, long long type, WarGrey::SCADA::double3& dot) override;
		void on_maniplation_complete(long long open_s, long long close_s) override;

	protected:
		void on_appdata(Windows::Foundation::Uri^ track, WarGrey::DTPM::DredgeTrack^ track_config) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ file) override {}

	private:
		void construct_line_if_necessary(unsigned int type);
		void clear_lines();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ haxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ track;

	private:
		WarGrey::DTPM::DredgeTrack^ preview_config;
		WarGrey::DTPM::DredgeTrack^ track_config;
		Windows::Foundation::Uri^ ms_appdata_config;

	private:
		class Line;
		WarGrey::DTPM::DigMaplet* master;
		WarGrey::DTPM::DredgeTracklet::Line* lines[_N(DredgeTrackType)];

	private:
		WarGrey::DTPM::ITrackDataSource* data_source;
		long long loading_timepoint;
		long long history_span;
		long long history_destination;
		double last_after_image_period;

	private:
		float width;
		float height;
	};
}
