#pragma once

Windows::Foundation::Numerics::float3x2 make_translation_matrix(float x, float y);

Windows::Foundation::Numerics::float3x2 make_rotation_matrix(float radians, float cx, float cy, float tx = 0.0F, float ty = 0.0F);
Windows::Foundation::Numerics::float3x2 make_rotation_matrix(double degrees, float cx, float cy, float tx = 0.0F, float ty = 0.0F);
