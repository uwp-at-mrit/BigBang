#pragma once

#include "graphlet/primitive.hpp"

#include "time.hpp"
#include "tongue.hpp"
#include "object.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class TableState { Realtime, History, _ };

	private struct TableCellStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground_color;

		float margin = -1.0F;
		float corner_radius = -1.0F;

		float align_fx = -1.0F;
		float align_fy = -1.0F;
	};

	typedef float (*resolve_column_width_percentage)(unsigned int idx, unsigned int column_count);
	typedef void (*prepare_cell_style)(unsigned int col_idx, long long row_salt, WarGrey::SCADA::TableCellStyle* style);

	private struct TableStyle {
		WarGrey::SCADA::resolve_column_width_percentage resolve_column_width_percentage = nullptr;
		WarGrey::SCADA::prepare_cell_style prepare_cell_style = nullptr;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ head_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ cell_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ status_font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_foreground_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_line_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cell_background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cell_foreground_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cell_line_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_foreground_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_background_hicolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_foreground_hicolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_info_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ status_line_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ head_col_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ head_row_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cell_col_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cell_row_line_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ status_line_style;

		float border_thickness = -1.0F;
		float head_row_line_thickness = -1.0F;
		float head_col_line_thickness = -1.0F;
		float cell_row_line_thickness = -1.0F;
		float cell_col_line_thickness = -1.0F;
		float status_line_thickness = -1.0F;

		float head_minheight_em = -1.0F;
		float cell_height_em = -1.0F;
		float status_height_em = -1.0F;

		float cell_corner_radius = -1.0F;
		float cell_margin = -1.0F;

		float status_corner_radius = -1.0F;
		float status_margin = -1.0F;
	};

	float resolve_average_column_width(unsigned int idx, unsigned int column_count);
	void prepare_default_cell_style(unsigned int idx, long long row_salt, WarGrey::SCADA::TableCellStyle* style);
	
	/************************************************************************************************/
	class TableBeing;

	private class ITableDataReceiver abstract {
	public:
		virtual void begin_maniplation_sequence() {}
		virtual void on_row_datum(long long request_count, long long nth, long long salt, Platform::String^ fields[], unsigned int n) = 0;
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

	private class ITableFilter abstract {
	public:
		virtual bool filter(long long salt) = 0;
	};

	/************************************************************************************************/
	private class ITablet abstract
		: public WarGrey::SCADA::IStatelet<WarGrey::SCADA::TableState, WarGrey::SCADA::TableStyle>
		, public WarGrey::SCADA::ITableDataReceiver {
	public:
		virtual ~ITablet() noexcept;

		ITablet(WarGrey::SCADA::ITableDataSource* src, float width, float height, long long history_max);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		void on_tap(float local_x, float local_y) override;
		void own_caret(bool yes) override;

	public:
		void on_row_datum(long long request_count, long long nth, long long salt, Platform::String^ fields[], unsigned int n) override;
		void on_maniplation_complete(long long request_count) override;

	public:
		unsigned long long count();
		void push_row(long long salt, Platform::String^ fields[]);
		void update_row(long long salt, Platform::String^ fields[]);

	public:
		void set_filter(WarGrey::SCADA::ITableFilter* filter, Platform::String^ on_label, Platform::String^ off_label = nullptr);
		void enable_filter(bool on_off);
		
	protected:
		void prepare_style(WarGrey::SCADA::TableState state, WarGrey::SCADA::TableStyle& style) override;
		void apply_style(WarGrey::SCADA::TableStyle& style) override;
		
	protected:
		void construct_table(Platform::String^ fields[], unsigned int count);
		
	private:
		void prepare_cell_style(WarGrey::SCADA::TableStyle& table_style, WarGrey::SCADA::TableCellStyle& cell_style);
		void update_statistics(WarGrey::SCADA::TableStyle& table_style);
		void update_description(WarGrey::SCADA::TableStyle& table_style);
		float resolve_filter_width();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ statistics;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ active_description;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ inactive_description;

	private:
		WarGrey::SCADA::TableBeing* table;
		unsigned int page_row_count;
		unsigned int page_index;

	private:
		WarGrey::SCADA::ITableFilter* filter;
		Platform::String^ on_description;
		Platform::String^ off_description;
		bool filter_actived;

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
			: ITablet(src, width, height, history_max), tongue(tongue) {}

	public:
		void construct() override {
			Platform::String^ fields[_N(Name)];

			for (unsigned int idx = 0; idx < _N(Name); idx++) {
				fields[idx] = speak(_E(Name, idx), this->tongue);
			}

			this->construct_table(fields, _N(Name));
		}

	private:
		Platform::String^ tongue;
	};
}
