#include "graphlet/vessellet.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
IVessellet::IVessellet() {
	this->enable_resizing(true);
}

/*************************************************************************************************/
float2 WarGrey::SCADA::vessel_point(double2 src, double2& gps, float2 s, float2* lt, float2* rb) {
	float x = float(src.x - gps.x) * s.x;
	float y = float(src.y - gps.y) * s.y;

	// NOTE that the map uses lefthand coordinate system, the xscale and yscale therefore should be interchanged
	// Stupid design, and/or stupid referenced codebase for lacking of explanation

	region_fuse_point(lt, rb, y, -x);

	return float2(y, -x);
}

CanvasGeometry^ WarGrey::SCADA::vessel_polygon(double2 src[], size_t count, double2& gps, float2 s, float2* lt, float2* rb) {
	Platform::Array<float2>^ vertexes = ref new Platform::Array<float2>((unsigned int)count);
	
	for (unsigned int idx = 0; idx < vertexes->Length; idx++) {
		vertexes->set(idx, vessel_point(src[idx], gps, s, lt, rb));
	}

	return CanvasGeometry::CreatePolygon(CanvasDevice::GetSharedDevice(), vertexes);
}

float WarGrey::SCADA::vessel_radius(float2& lt, float2& rb) {
	float rlt = points_distance(lt.x, lt.y, 0.0F, 0.0F);
	float rrt = points_distance(rb.x, lt.y, 0.0F, 0.0F);
	float rlb = points_distance(lt.x, rb.y, 0.0F, 0.0F);
	float rrb = points_distance(rb.x, rb.y, 0.0F, 0.0F);

	return flmax(flmax(rlt, rrt), flmax(rlb, rrb));
}

float WarGrey::SCADA::vessel_radius(float2& lt, float2& rb, double2& gps) {
	float gx = float(gps.x);
	float gy = float(gps.y);
	float rlt = points_distance(lt.x, lt.y, gx, gy);
	float rrt = points_distance(rb.x, lt.y, gx, gy);
	float rlb = points_distance(lt.x, rb.y, gx, gy);
	float rrb = points_distance(rb.x, rb.y, gx, gy);

	return flmax(flmax(rlt, rrt), flmax(rlb, rrb));
}
