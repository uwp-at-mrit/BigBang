#pragma once

Windows::UI::Color rgba(int hex, double alpha = 1.0);
Windows::UI::Color rgba(Windows::UI::Color src, double alpha = 1.0);
Windows::UI::Color rgba(double red, double green, double blue, double alpha = 1.0);

Windows::UI::Color hsva(double hue, double saturation, double value, double alpha = 1.0);
Windows::UI::Color hsla(double hue, double saturation, double lightness, double alpha = 1.0);
Windows::UI::Color hsia(double hue, double saturation, double intensity, double alpha = 1.0);

Windows::UI::Color contrast_color(Windows::UI::Color& src);
