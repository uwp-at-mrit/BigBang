#pragma once

#include "graphlet/primitive.hpp"

#include "time.hpp"
#include "tongue.hpp"
#include "object.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	typedef float (*resolve_column_width_percentage)(unsigned int idx, unsigned int column_count);
	typedef void (*fill_cell_alignment)(unsigned int idx, float* fx, float* fy);

	class TableColumn;

	private enum class TableState { Realtime, History, _ };

	private struct TableStyle {
		WarGrey::SCADA::resolve_column_width_percentage resolve_column_width_percentage = nullptr;
		WarGrey::SCADA::fill_cell_alignment fill_cell_alignment = nullptr;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ head_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ cell_font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_foreground_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_line_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cell_line_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ head_col_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ head_row_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cell_col_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cell_row_line_style;

		float border_thickness = -1.0F;
		float head_row_line_thickness = -1.0F;
		float head_col_line_thickness = -1.0F;
		float cell_row_line_thickness = -1.0F;
		float cell_col_line_thickness = -1.0F;

		float head_minheight_em = -1.0F;
		float cell_height_em = -1.0F;

		float cell_margin = -1.0F;
		float cell_corner_radius = -1.0F;
	};


	float resolve_average_column_width(unsigned int idx, unsigned int column_count);

	void fill_cell_alignment_lc(unsigned int idx, float* fx, float* fy);
	void fill_cell_alignment_cc(unsigned int idx, float* fx, float* fy);
	void fill_cell_alignment_rc(unsigned int idx, float* fx, float* fy);

	/************************************************************************************************/
	private class ITableDataReceiver abstract {
	public:
		virtual void begin_maniplation_sequence() {}
		virtual void on_row_datum(long long request_count, long long nth, Platform::String^ fields[], unsigned int n) = 0;
		virtual void end_maniplation_sequence() {}

	public:
		virtual void on_maniplation_complete(long long request_count) {}
	};

	private class ITableDataSource abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool ready() = 0;
		virtual bool loading() = 0;
		virtual void cancel() = 0;

	public:
		virtual void load(WarGrey::SCADA::ITableDataReceiver* receiver, long long request_count) = 0;

	protected:
		~ITableDataSource() noexcept {}
	};

	/************************************************************************************************/
	private class ITablet abstract
		: public WarGrey::SCADA::IStatelet<WarGrey::SCADA::TableState, WarGrey::SCADA::TableStyle>
		, public WarGrey::SCADA::ITableDataReceiver {
	public:
		virtual ~ITablet() noexcept;

		ITablet(WarGrey::SCADA::ITableDataSource* src, float width, float height, unsigned int column_count, long long history_max);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		void own_caret(bool yes) override;

	public:
		void on_row_datum(long long request_count, long long nth, Platform::String^ fields[], unsigned int n) override;
		void on_maniplation_complete(long long request_count) override;

	public:
		void set_row(Platform::String^ fields[]);
		void set_history_interval(long long open_s, long long close_s, bool force = false);
		void scroll_to_timepoint(long long timepoint_ms, float visual_boundary_proportion_of_series_interval = 1.5F);

	protected:
		void prepare_style(WarGrey::SCADA::TableState state, WarGrey::SCADA::TableStyle& style) override;
		void apply_style(WarGrey::SCADA::TableStyle& style) override;
		
	protected:
		void construct_column(unsigned int idx, Platform::String^ name);
		
	private:
		WarGrey::SCADA::TableColumn* columns;
		unsigned int column_count;
		unsigned int page_row_count;

	private:
		float width;
		float height;
		
	private:
		WarGrey::SCADA::ITableDataSource* data_source;
		long long history_max;
		bool request_loading;
	};

	template<typename Name>
	private class Tablet : public WarGrey::SCADA::ITablet {
	public:
		Tablet(Platform::String^ tongue, float width, float height, long long history_max = 1024)
			: Tablet(tongue, nullptr, width, height, history_max) {}

		Tablet(Platform::String^ tongue, WarGrey::SCADA::ITableDataSource* src, float width, float height, long long history_max = 1024)
			: ITablet(src, width, height, _N(Name), history_max), tongue(tongue) {}

	public:
		void construct() override {
			for (unsigned int idx = 0; idx < _N(Name); idx++) {
				this->construct_column(idx, speak(_E(Name, idx), this->tongue));
			}
		}

	private:
		Platform::String^ tongue;
	};
}
