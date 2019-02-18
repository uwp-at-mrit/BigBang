#pragma once
#pragma warning (disable: 4250)

#include "graphlet/textlet.hpp"

namespace WarGrey::SCADA {
	private enum class DatePickerState { Default, Input, _ };

	private struct DatePickerStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ datetime_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ datetime_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ datetime_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ datetime_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ separator_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ separator_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ separator_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ separator_background_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ caret_color;

		/**
		 * By design,
		 *  if these numerical parameters are less than 0,
		 *  they are considered not set.
		 */

		float minimize_label_width = -1.0F;
		float label_xfraction = -1.0F;

		float datetime_leading_space = -1.0F;
	};

	private class DatePickerlet abstract
		: public virtual WarGrey::SCADA::ITextlet
		, public virtual WarGrey::SCADA::IValuelet<long long>
		, public virtual WarGrey::SCADA::IStatelet<WarGrey::SCADA::DatePickerState, WarGrey::SCADA::DatePickerStyle> {
	public:
		DatePickerlet(long long timepoint_s, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr);

		DatePickerlet(WarGrey::SCADA::DatePickerState default_state, long long timepoint_s,
			Platform::String^ label = nullptr, Platform::String^ subscript = nullptr);

		DatePickerlet(WarGrey::SCADA::DatePickerStyle& default_style, long long timepoint_s,
			Platform::String^ label = nullptr, Platform::String^ subscript = nullptr);

		DatePickerlet(WarGrey::SCADA::DatePickerState default_state, WarGrey::SCADA::DatePickerStyle& default_style, long long timepoint_s,
			Platform::String^ label = nullptr, Platform::String^ subscript = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;

	protected:
		void prepare_style(WarGrey::SCADA::DatePickerState status, WarGrey::SCADA::DatePickerStyle& style) override;
		void apply_style(WarGrey::SCADA::DatePickerStyle& style) override;
		void on_state_changed(WarGrey::SCADA::DatePickerState status) override;

	protected:
		long long guarded_value(long long value) override;
		void on_value_changed(long long timepoint) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ datetime_layouts[6];
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ date_separator_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ time_separator_layout;
		WarGrey::SCADA::TextExtent datetime_boxes[6];
		WarGrey::SCADA::TextExtent date_separator_box;
		WarGrey::SCADA::TextExtent time_separator_box;
		WarGrey::SCADA::TextExtent number09_box; // for fonts that number chars do not have same height

	private:
		int caret_index;
		long long timepoint0;
	};
}
