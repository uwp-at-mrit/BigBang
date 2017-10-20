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
#include "rsyslog.hpp"

static inline int rgb_to_hex(Color rgb) {
    return (rgb.R << 16) | (rgb.G << 8) | rgb.B;
}

void test_colorspace() {
    double samples[][14]{
        /*01*/{ 1.000, 0.000, 0.000,   0.0,   0.0, 1.000, 1.000, 1.000, 0.500, 0.333, 0.299, 1.000, 1.000, 1.000 },
        /*02*/{ 0.750, 0.750, 0.000,  60.0,  60.0, 0.750, 0.750, 0.750, 0.375, 0.500, 0.664, 1.000, 1.000, 1.000 },
        /*03*/{ 0.000, 0.500, 0.000, 120.0, 120.0, 0.500, 0.500, 0.500, 0.250, 0.167, 0.293, 1.000, 1.000, 1.000 },
        /*04*/{ 0.500, 1.000, 1.000, 180.0, 180.0, 0.500, 0.500, 1.000, 0.750, 0.833, 0.850, 0.500, 1.000, 0.400 },
        /*05*/{ 0.500, 0.500, 1.000, 240.0, 240.0, 0.500, 0.500, 1.000, 0.750, 0.667, 0.557, 0.500, 1.000, 0.250 },
        /*06*/{ 0.750, 0.250, 0.750, 300.0, 300.0, 0.500, 0.500, 0.750, 0.500, 0.583, 0.457, 0.667, 0.500, 0.571 },
        /*07*/{ 0.628, 0.643, 0.142,  61.8,  61.5, 0.501, 0.494, 0.643, 0.393, 0.471, 0.581, 0.779, 0.638, 0.699 },
        /*08*/{ 0.255, 0.104, 0.918, 251.1, 250.0, 0.814, 0.750, 0.918, 0.511, 0.426, 0.242, 0.887, 0.832, 0.756 },
        /*09*/{ 0.116, 0.675, 0.255, 134.9, 133.8, 0.559, 0.504, 0.675, 0.396, 0.349, 0.460, 0.828, 0.707, 0.667 },
        /*10*/{ 0.941, 0.785, 0.053,  49.5,  50.5, 0.888, 0.821, 0.941, 0.497, 0.593, 0.748, 0.944, 0.893, 0.911 },
        /*11*/{ 0.704, 0.187, 0.897, 283.7, 284.8, 0.710, 0.636, 0.897, 0.542, 0.596, 0.423, 0.792, 0.775, 0.686 },
        /*12*/{ 0.931, 0.463, 0.316,  14.3,  13.2, 0.615, 0.556, 0.931, 0.624, 0.570, 0.586, 0.661, 0.817, 0.446 },
        /*13*/{ 0.998, 0.974, 0.532,  56.9,  57.4, 0.466, 0.454, 0.998, 0.765, 0.835, 0.931, 0.467, 0.991, 0.363 },
        /*14*/{ 0.099, 0.795, 0.591, 162.4, 163.4, 0.696, 0.620, 0.795, 0.447, 0.495, 0.564, 0.875, 0.779, 0.800 },
        /*15*/{ 0.211, 0.149, 0.597, 248.3, 247.3, 0.448, 0.420, 0.597, 0.373, 0.319, 0.219, 0.750, 0.601, 0.533 },
        /*16*/{ 0.495, 0.493, 0.721, 240.5, 240.4, 0.228, 0.227, 0.721, 0.607, 0.570, 0.520, 0.316, 0.290, 0.135 }
    };

    for (int i = 0; i < sizeof(samples) / sizeof(double[14]); i++) {
        int rgb = rgb_to_hex(rgba(samples[i][0], samples[i][1],  samples[i][2]));
        int hsv = rgb_to_hex(hsva(samples[i][3], samples[i][11], samples[i][7]));
        int hsl = rgb_to_hex(hsla(samples[i][3], samples[i][12], samples[i][8]));
        int hsi = rgb_to_hex(hsia(samples[i][3], samples[i][13], samples[i][9]));
        
        Platform::String^ frmt = "[%02d]RGB:%06X"
            + " HSV:%06" + ((rgb == hsv) ? "X" : "x")
            + " HSL:%06" + ((rgb == hsl) ? "X" : "x")
            + " HSI:%06" + ((rgb == hsi) ? "X" : "x");

        rsyslog(frmt->Data(), i + 1, rgb, hsv, hsl, hsi);
    }
}
