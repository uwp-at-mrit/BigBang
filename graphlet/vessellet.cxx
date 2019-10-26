#include "graphlet/vessellet.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

static void vessel_bow_transform(double bow, float* cosbow, float* sinbow) {
	float radians = degrees_to_radians(bow);

	(*cosbow) = flcos(radians);
	(*sinbow) = flsin(radians);
}

static float2 vessel_point_on_screen(double src_x, double src_y, double2& gps, float2 s, float cosbow, float sinbow, float2* lt, float2* rb) {
	float geo_x = float(src_x - gps.x) * s.x;
	float geo_y = float(src_y - gps.y) * s.y;
	float x0 = geo_y;
	float y0 = -geo_x;
	float x = x0 * cosbow - y0 * sinbow;
	float y = x0 * sinbow + y0 * cosbow;

	// NOTE that the map uses lefthand coordinate system, the xscale and yscale therefore should be interchanged
	// Stupid design, and/or stupid referenced codebase for lacking of explanation

	region_fuse_point(lt, rb, x, y);

	return float2(x, y);
}

/*************************************************************************************************/
IVessellet::IVessellet() {
	this->enable_resizing(false); // Affine transformation is difficulty
}

void IVessellet::scale(double s) {
	Size os = this->original_size();

	this->moor(GraphletAnchor::CC);
	this->resize(float(os.Width * s), float(os.Height * s));
}

void IVessellet::clear_boundary() {
	this->lt.x = +infinity_f;
	this->lt.y = +infinity_f;
	this->rb.x = -infinity_f;
	this->rb.y = -infinity_f;
}

/*************************************************************************************************/
float2 WarGrey::SCADA::vessel_point(double2& src, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	return vessel_point(src.x, src.y, gps, s, bow, lt, rb);
}

float2 WarGrey::SCADA::vessel_point(double x, double y, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	return vessel_point_on_screen(x, y, gps, s, cosbow, sinbow, lt, rb);
}

CanvasGeometry^ WarGrey::SCADA::vessel_polygon(double2 src[], size_t count, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	Platform::Array<float2>^ vertexes = ref new Platform::Array<float2>((unsigned int)count);
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	for (unsigned int idx = 0; idx < vertexes->Length; idx++) {
		vertexes->set(idx, vessel_point_on_screen(src[idx].x, src[idx].y, gps, s, cosbow, sinbow, lt, rb));
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
