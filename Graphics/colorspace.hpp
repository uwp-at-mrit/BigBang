#pragma once

Windows::UI::Color hsv(double hue, double saturation, double value);
Windows::UI::Color hsl(double hue, double saturation, double lightness);
Windows::UI::Color hsi(double hue, double saturation, double intensity);

void test_colorspace();
