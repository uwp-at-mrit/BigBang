#define _USE_MATH_DEFINES
#include <algorithm>

#include "colorspace.hpp"

using namespace Windows::UI;

#define UCHAR(v) ((unsigned char)std::round(v * 255.0))

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

Color rgba(int hex, double a) {
    auto r = (unsigned char)((hex >> 16) & 0xFF);
    auto g = (unsigned char)((hex >> 8) & 0xFF);
    auto b = (unsigned char)(hex & 0xFF);

    return ColorHelper::FromArgb(UCHAR(a), r, g, b);
}

Color rgba(Color src, double a) {
    return ColorHelper::FromArgb(UCHAR(a), src.R, src.G, src.B);
}

Color rgba(double r, double g, double b, double a) {
    return ColorHelper::FromArgb(UCHAR(a), UCHAR(r), UCHAR(g), UCHAR(b));
}

Color hsva(double hue, double saturation, double value, double alpha) {
    double chroma = saturation * value;
    double m = value - chroma;
    
    return hue_to_rgba(hue, chroma, m, alpha);
}

Color hsla(double hue, double saturation, double lightness, double alpha) {
    double chroma = saturation * (1.0 - std::abs(lightness * 2.0 - 1.0));
    double m = lightness - chroma * 0.5;
    
    return hue_to_rgba(hue, chroma, m, alpha);
}

Color hsia(double hue, double saturation, double intensity, double alpha) {
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

/*************************************************************************************************/
Color contrast_color(Color& src) {
	// NOTE: human eye favors green color... 
	double perceptive_luminance = 1.0 - (double(src.R) * 0.299 + double(src.G) * 0.587 + double(src.B) * 0.114) / 255.0;

	if (perceptive_luminance < 0.5) {
		return Colors::Black;
	} else {
		return Colors::White;
	}
}
