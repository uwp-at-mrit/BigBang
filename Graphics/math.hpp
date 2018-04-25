#pragma once

float viewport_fit_scaling(Windows::Foundation::Size& src_size, float target_width, float target_height);
float viewport_fit_scaling(float src_width, float src_height, float target_width, float target_height);

void circle_point(float radius, double degrees, float* x, float* y);
void line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y);
