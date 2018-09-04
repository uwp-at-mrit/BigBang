#define _USE_MATH_DEFINES
#include <algorithm>
#include <cmath>

#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

#define UCHAR(v) ((unsigned char)std::round(v * 255.0))

static const unsigned int REPEAT_PEN_COLOR_COUNT = 128U;
static const unsigned int REPEAT_BRUSH_COLOR_COUNT = 128U;

static double integer_to_gray_value(int n) {
	return double(std::abs(n) % 8) / 7.0;
}

static int integer_to_hue(int n) {
	int i = std::abs(n) / 6 * 18;
	
	switch (std::abs(n) % 6) {
	case 0:  i += 0; break;
	case 1:  i += 2; break;
	case 2:  i += 4; break;
	case 3:  i += 1; break;
	case 4:  i += 3; break;
	default: i += 5;
	}

	return (i * 59) % 360;
}

static Color integer_gray_pen_color(int idx) {
	double r = std::pow(integer_to_gray_value(idx), 0.75) * 128.0;
	unsigned char c = (unsigned char)std::round(r);

	return ColorHelper::FromArgb((unsigned char)(255), c, c, c);
}

static Color integer_pen_color(int idx) {
	double hue = double(integer_to_hue(idx));
	double spi = std::sin((hue / 360.0) * 3.0 * M_PI);
	double h = hue - spi * 25.0;
	double s = 1.0;
	double v = spi / 6.0 + 0.5;

	return hsva(h, s, v);
}

static Color integer_gray_brush_color(int idx) {
	double r = std::pow(std::max((integer_to_gray_value(idx) - 1.0), 0.0), 0.75) * 128.0 + 127.0;
	unsigned char c = (unsigned char)std::round(r);

	return ColorHelper::FromArgb((unsigned char)(255), c, c, c);
}

static Color integer_brush_color(int idx) {
	double hue = double(integer_to_hue(idx));
	double y = (std::sqrt(hue / 6.0 + 2.0) - std::sqrt(2.0)) / (std::sqrt(8.0) - std::sqrt(2.0)) * 6.0;
	double h = hue - std::sin((y / 6.0) * 3.0 * M_PI) * 15.0;
	double s = std::sin((hue / 360.0) * 2.0 * M_PI) * 3.0 / 32.0 + 3.0 / 16.0;
	double v = 1.0;

	return hsva(h, s, v);
}

static Color hue_to_rgba(double hue, double chroma, double m, double a) {
    double r = m;
    double g = m;
    double b = m;
    
    if (!std::isnan(hue)) {
        double hue_60 = hue / 60.0;
        double flhue = std::floor(hue_60);
        int fxhue = int(flhue);
        double x = chroma * (1.0 - std::abs(double(fxhue % 2) - (flhue - hue_60) - 1.0));
        
        switch (fxhue) {
        case 0: r += chroma; g += x; break;
        case 1: r += x; g += chroma; break;
        case 2: g += chroma; b += x; break;
        case 3: g += x; b += chroma; break;
        case 4: r += x; b += chroma; break;
        case 5: r += chroma; b += x; break;
        }
    }

    return rgba(r, g, b, a);
}

static double color_to_hue(Color color, double* M, double* m, double *chroma) {
	double hue = std::nan("zero chroma");
	char r = color.R;
	char g = color.G;
	char b = color.B;
	char fxM = std::max(r, std::max(g, b));
	char fxm = std::min(r, std::min(g, b));
	
	if (fxM > fxm) { // the same as: chroma == 0.0
		if (fxM == g) {
			hue = 60.0 * (2.0 + (double(b - r) / (*chroma)));
		} else if (fxM == b) {
			hue = 60.0 * (4.0 + (double(r - g) / (*chroma)));
		} else if (g < b) {
			hue = 60.0 * (6.0 + (double(g - b) / (*chroma)));
		} else {
			hue = 60.0 * (double(g - b) / (*chroma));
		}
	}

	(*M) = double(fxM);
	(*m) = double(fxm);
	(*chroma) = (*M) - (*m);

	return hue;
}

static Color hsi_sector_to_rgb(double hue, double saturation, double intensity, char color_component, double alpha) {
    double cosH_60H = 2.0; // if hue == 0.0 or hue == 120.0;

    if ((hue != 0.0) && (hue != 120.0)) {
        double H = hue * (M_PI / 180.0);
        cosH_60H = std::cos(H) / std::cos(M_PI / 3.0 - H);
    }

    {
        double major = intensity * (1.0 + saturation * cosH_60H);
        double midor = intensity * (1.0 - saturation);
        double minor = (intensity * 3.0) - (major + midor);

        switch (color_component) {
        case 'r': return rgba(major, minor, midor, alpha); break;
        case 'g': return rgba(midor, major, minor, alpha); break;
        default:  return rgba(minor, midor, major, alpha); break;
        }
    }
}

static inline char do_scale(unsigned char src, double s) {
	unsigned char dest = src;

	if (s > 1.0) {
		dest = 255 - (unsigned char)(std::floor(double(255 - src) / s));
	} else {
		dest = std::min((unsigned char)(255), (unsigned char)(std::floor(double(src) * s)));
	}

	return dest;
}

/*************************************************************************************************/
unsigned char WarGrey::SCADA::color_double_to_char(double c) {
	return UCHAR(c);
}

unsigned int WarGrey::SCADA::color_to_hexadecimal(Color& c) {
	return (c.R << 16) | (c.G << 8) | c.B; 
}

/*************************************************************************************************/
Color WarGrey::SCADA::rgba(unsigned int hex, double a) {
    auto r = (unsigned char)((hex >> 16) & 0xFF);
    auto g = (unsigned char)((hex >> 8) & 0xFF);
    auto b = (unsigned char)(hex & 0xFF);

    return ColorHelper::FromArgb(UCHAR(a), r, g, b);
}

Color WarGrey::SCADA::rgba(Color& src, double a) {
    return ColorHelper::FromArgb(UCHAR(a), src.R, src.G, src.B);
}

Color WarGrey::SCADA::rgba(double r, double g, double b, double a) {
    return ColorHelper::FromArgb(UCHAR(a), UCHAR(r), UCHAR(g), UCHAR(b));
}

/*************************************************************************************************/
Color WarGrey::SCADA::hsva(double hue, double saturation, double value, double alpha) {
    double chroma = saturation * value;
    double m = value - chroma;
    
    return hue_to_rgba(hue, chroma, m, alpha);
}

void WarGrey::SCADA::fill_hsv_color(Color& color, double* hue, double* saturation, double* value) {
	double M, m, chroma;
	
	(*hue) = color_to_hue(color, &M, &m, &chroma);
	(*saturation) = ((M == 0.0) ? 0.0 : (chroma / M));
	(*value) = M;
}

void WarGrey::SCADA::fill_hsv_color(unsigned int hex, double* hue, double* saturation, double* value) {
	fill_hsv_color(rgba(hex), hue, saturation, value);
}

/*************************************************************************************************/
Color WarGrey::SCADA::hsla(double hue, double saturation, double lightness, double alpha) {
    double chroma = saturation * (1.0 - std::abs(lightness * 2.0 - 1.0));
    double m = lightness - chroma * 0.5;
    
    return hue_to_rgba(hue, chroma, m, alpha);
}

void WarGrey::SCADA::fill_hsl_color(Color& color, double* hue, double* saturation, double* lightness) {
	double M, m, chroma;
	double h = color_to_hue(color, &M, &m, &chroma);
	double L = (M + m) * 0.5;

	(*hue) = h;
	(*saturation) = ((L == 1.0) ? 0.0 : (chroma / (1.0 - std::abs(2.0 * L - 1.0))));
	(*lightness) = L;
}

void WarGrey::SCADA::fill_hsl_color(unsigned int hex, double* hue, double* saturation, double* lightness) {
	fill_hsl_color(rgba(hex), hue, saturation, lightness);
}

/*************************************************************************************************/
Color WarGrey::SCADA::hsia(double hue, double saturation, double intensity, double alpha) {
    if ((saturation == 0.0) || std::isnan(saturation)) {
        return rgba(intensity, intensity, intensity, alpha);
    } else if (hue < 120.0) {
        return hsi_sector_to_rgb(hue, saturation, intensity, 'r', alpha);
    } else if (hue < 240.0) {
        return hsi_sector_to_rgb(hue - 120.0, saturation, intensity, 'g', alpha);
    } else {
        return hsi_sector_to_rgb(hue - 240.0, saturation, intensity, 'b', alpha);
    }
}

void WarGrey::SCADA::fill_hsi_color(Color& color, double* hue, double* saturation, double* intensity) {
	double r = double(color.R);
	double g = double(color.G);
	double b = double(color.B);
	double alpha = (r - (g + b) * 0.5);
	double beta = std::sqrt(((r - g) * (r - g)) + ((r - b) * (g - b)));
	double h = std::acos(alpha / beta) * 180.0 / M_PI;
	double I = (r + g + b) / 3.0;

	(*hue) = ((b > g) ? (360 - h) : h);
	(*saturation) = ((I == 0.0) ? 0.0 : (1.0 - (std::min(r, std::min(g, b)) / I)));
	(*intensity) = I;
}

void WarGrey::SCADA::fill_hsi_color(unsigned int hex, double* hue, double* saturation, double* intensity) {
	fill_hsl_color(rgba(hex), hue, saturation, intensity);
}

/*************************************************************************************************/
Color WarGrey::SCADA::contrast_color(Color& src) {
	// NOTE: human eye favors green color... 
	double perceptive_luminance = 1.0 - (double(src.R) * 0.299 + double(src.G) * 0.587 + double(src.B) * 0.114) / 255.0;

	if (perceptive_luminance < 0.5) {
		return Colors::Black;
	} else {
		return Colors::White;
	}
}

Color WarGrey::SCADA::scale_color(Color& src, double scale) {
	double s = std::max(scale, 0.0);

	return ColorHelper::FromArgb(src.A, do_scale(src.R, s), do_scale(src.G, s), do_scale(src.B, s));
}

Color WarGrey::SCADA::darken_color(Windows::UI::Color& src) {
	return scale_color(src, 0.5);
}

Color WarGrey::SCADA::lighten_color(Windows::UI::Color& src) {
	return scale_color(src, 2.0);
}

/*************************************************************************************************/
Color WarGrey::SCADA::lookup_light_color(unsigned int idx0) {
	unsigned int gray_idx = REPEAT_BRUSH_COLOR_COUNT - 7;
	unsigned int idx = idx0 % REPEAT_BRUSH_COLOR_COUNT;
	Color c;

	if (idx == 0) {
		c = integer_gray_brush_color(0);
	} else if (idx < gray_idx) {
		c = integer_brush_color(idx - 1);
	} else {
		c = integer_gray_brush_color(7 - (idx - gray_idx));
	}

	return c;
}

Color WarGrey::SCADA::lookup_dark_color(unsigned int idx0) {
	unsigned int gray_idx = REPEAT_PEN_COLOR_COUNT - 7;
	unsigned int idx = idx0 % REPEAT_PEN_COLOR_COUNT;
	Color c;

	if (idx == 0) {
		c = integer_gray_pen_color(0);
	} else if (idx < gray_idx) {
		c = integer_pen_color(idx - 1);
	} else {
		c = integer_gray_pen_color(7 - (idx - gray_idx));
	}

	return c;
}
