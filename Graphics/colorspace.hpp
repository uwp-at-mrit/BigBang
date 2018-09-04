#pragma once

namespace WarGrey::SCADA {
	unsigned char color_double_to_char(double c);
	unsigned int color_to_hexadecimal(Windows::UI::Color& c);

	Windows::UI::Color rgba(unsigned int hex, double alpha = 1.0);
	Windows::UI::Color rgba(Windows::UI::Color& src, double alpha = 1.0);
	Windows::UI::Color rgba(double red, double green, double blue, double alpha = 1.0);

	Windows::UI::Color hsva(double hue, double saturation, double value, double alpha = 1.0);
	void fill_hsv_color(unsigned int hex, double* hue, double* saturation, double* value);
	void fill_hsv_color(Windows::UI::Color& c, double* hue, double* saturation, double* value);

	Windows::UI::Color hsla(double hue, double saturation, double lightness, double alpha = 1.0);
	void fill_hsl_color(unsigned int hex, double* hue, double* saturation, double* lightness);
	void fill_hsl_color(Windows::UI::Color& c, double* hue, double* saturation, double* lightness);

	Windows::UI::Color hsia(double hue, double saturation, double intensity, double alpha = 1.0);
	void fill_hsi_color(unsigned int hex, double* hue, double* saturation, double* intensity);
	void fill_hsi_color(Windows::UI::Color& c, double* hue, double* saturation, double* intensity);

	Windows::UI::Color contrast_color(Windows::UI::Color& src);
	Windows::UI::Color scale_color(Windows::UI::Color& src, double scale);
	Windows::UI::Color darken_color(Windows::UI::Color& src);
	Windows::UI::Color lighten_color(Windows::UI::Color& src);

	Windows::UI::Color lookup_dark_color(unsigned int idx);
	Windows::UI::Color lookup_light_color(unsigned int idx);
}
