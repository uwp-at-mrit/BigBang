#pragma once
#pragma warning (disable: 4250)

#include "graphlet/primitive.hpp"
#include "brushes.hxx"
#include "text.hpp"

namespace WarGrey::SCADA {
	private enum class DatePickerState { Normal, Input, Highlight, _ };

	private struct DatePickerStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ number_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_background_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ caret_color;

		/**
		 * By design,
		 *  if these numerical parameters are less than 0,
		 *  they are considered not set.
		 */

		float minimize_label_width = -1.0F;
		float label_xfraction = -1.0F;

		float minimize_number_width = -1.0F;
		float number_xfraction = -1.0F;

		float number_leading_space = -1.0F;
		float number_trailing_space = -1.0F;

		int precision = -1;
	};

	private class DatePicklet : public virtual WarGrey::SCADA::IStatelet<WarGrey::SCADA::DatePickerState, WarGrey::SCADA::DatePickerStyle> {
	public:
		DatePicklet(WarGrey::SCADA::DatePickerState default_state, Platform::String^ unit,
			Platform::String^ label, Platform::String^ subscript);

		DatePicklet(WarGrey::SCADA::DatePickerState default_state, WarGrey::SCADA::DatePickerStyle& default_style,
			Platform::String^ unit, Platform::String^ label, Platform::String^ subscript);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		void own_caret(bool is_own) override;

	public:
		void set_maximum(long double max);
		void set_input_number(long double n, int precision = 2);
		long double get_input_number();

	protected:
		void prepare_style(WarGrey::SCADA::DatePickerState status, WarGrey::SCADA::DatePickerStyle& style) override;
		void apply_style(WarGrey::SCADA::DatePickerStyle& style) override;
		void on_state_changed(WarGrey::SCADA::DatePickerState status) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caret_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ number_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unit_layout;
		WarGrey::SCADA::TextExtent caret_box;
		WarGrey::SCADA::TextExtent number_box;
		WarGrey::SCADA::TextExtent unit_box;
		WarGrey::SCADA::TextExtent number09_box; // for fonts that number chars do not have same height

	private:
		Platform::String^ input_number;
		Platform::String^ number;
		Platform::String^ unit;

	private:
		bool flashing;
		long double maximum;
		int decimal_position;
	};
}
