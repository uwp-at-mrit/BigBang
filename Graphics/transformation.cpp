#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "transformation.hpp"

using namespace Windows::Foundation::Numerics;

float3x2 WarGrey::SCADA::make_translation_matrix(float x, float y) {
	return make_float3x2_translation(float2(x, y));
}

float3x2 WarGrey::SCADA::make_scale_matrix(float sx, float sy) {
	return make_float3x2_scale(float2(sx, sy));
}

float3x2 WarGrey::SCADA::make_rotation_matrix(float radians, float cx, float cy, float tx, float ty) {
	float3x2 rt = make_float3x2_rotation(radians, float2(cx, cy));

	// TODO: find a more elegant way to combine translation and rotation.
	rt.m31 += tx;
	rt.m32 += ty;

	return rt;
}

float3x2 WarGrey::SCADA::make_rotation_matrix(double degrees, float cx, float cy, float tx, float ty) {
	return make_rotation_matrix(float(degrees * M_PI / 180.0), cx, cy, tx, ty);
}
