#include "graphlet/vessellet.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

static inline void vessel_bow_transform(double bow, float* cosbow, float* sinbow) {
	float radians = degrees_to_radians(bow);

	(*cosbow) = flcos(radians);
	(*sinbow) = flsin(radians);
}

static inline void vessel_body_transform(double2& src, double2& base, double2& src_sign, double* dotx, double* doty) {
	(*dotx) = base.x + src.x * src_sign.x;
	(*doty) = base.y + src.y * src_sign.y;
}

static inline double2 vessel_point_on_map(double2& o, double src_x, double src_y, double2& gps, float cosbow, float sinbow) {
	double geo_x = src_x - gps.x;
	double geo_y = src_y - gps.y;
	double x = geo_x * cosbow - geo_y * sinbow;
	double y = geo_x * sinbow + geo_y * cosbow;
	
	return double2(x + o.x, y + o.y);
}

static inline float2 vessel_point_on_screen(double src_x, double src_y, double2& gps, float2 s, float cosbow, float sinbow, float2* lt, float2* rb) {
	float geo_x = float(src_x - gps.x) * s.x;
	float geo_y = float(src_y - gps.y) * s.y;
	float x0 = geo_y;
	float y0 = -geo_x;
	float x = x0 * cosbow - y0 * sinbow;
	float y = x0 * sinbow + y0 * cosbow;

	// NOTE that the map uses YX-axis coordinate system, the xscale and yscale therefore should be interchanged
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
	this->clear_moor(); // the vessel may already scaled in which case the notify_updated() will not be invoked and hence manually clear the moor. 
}

void IVessellet::clear_boundary() {
	region_fuse_reset(&this->lt, &this->rb);
}

void IVessellet::resolve_radius() {
	this->xradius = vessel_radius(this->lt, this->rb);
	this->yradius = this->xradius;
}

/*************************************************************************************************/
double2 WarGrey::DTPM::vessel_geo_point(double2& o, double2& src, double2& gps, double bow) {
	return vessel_geo_point(o, src.x, src.y, gps, bow);
}

double2 WarGrey::DTPM::vessel_geo_point(double2& o, double x, double y, double2& gps, double bow) {
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	return vessel_point_on_map(o, x, y, gps, cosbow, sinbow);
}

double2 WarGrey::DTPM::vessel_geo_point(double2& o, double2& src, double2& base, double2& src_sign, double2& gps, double bow) {
	double dotx, doty;
	
	vessel_body_transform(src, base, src_sign, &dotx, &doty);

	return vessel_geo_point(o, dotx, doty, gps, bow);
}

float2 WarGrey::DTPM::vessel_point(double2& src, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	return vessel_point(src.x, src.y, gps, s, bow, lt, rb);
}

float2 WarGrey::DTPM::vessel_point(double2& src, double2& base, double2& src_sign, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	double dotx, doty;

	vessel_body_transform(src, base, src_sign, &dotx, &doty);

	return vessel_point(dotx, doty, gps, s, bow, lt, rb);
}

float2 WarGrey::DTPM::vessel_point(double x, double y, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	return vessel_point_on_screen(x, y, gps, s, cosbow, sinbow, lt, rb);
}

CanvasGeometry^ WarGrey::DTPM::vessel_polygon(double2 src[], size_t count, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	Platform::Array<float2>^ vertexes = ref new Platform::Array<float2>((unsigned int)count);
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	for (unsigned int idx = 0; idx < vertexes->Length; idx++) {
		vertexes->set(idx, vessel_point_on_screen(src[idx].x, src[idx].y, gps, s, cosbow, sinbow, lt, rb));
	}

	return CanvasGeometry::CreatePolygon(CanvasDevice::GetSharedDevice(), vertexes);
}

CanvasGeometry^ WarGrey::DTPM::vessel_polygon(double2 src[], size_t count, double2& base, double2& src_sign, double2& gps, float2& s, double bow, float2* lt, float2* rb) {
	Platform::Array<float2>^ vertexes = ref new Platform::Array<float2>((unsigned int)count);
	float cosbow, sinbow;

	vessel_bow_transform(bow, &cosbow, &sinbow);

	for (unsigned int idx = 0; idx < vertexes->Length; idx++) {
		double dotx = base.x + src[idx].x * src_sign.x;
		double doty = base.y + src[idx].y * src_sign.y;

		vertexes->set(idx, vessel_point_on_screen(dotx, doty, gps, s, cosbow, sinbow, lt, rb));
	}

	return CanvasGeometry::CreatePolygon(CanvasDevice::GetSharedDevice(), vertexes);
}

float WarGrey::DTPM::vessel_radius(float2& lt, float2& rb) {
	float rlt2 = points_distance_squared(lt.x, lt.y, 0.0F, 0.0F);
	float rrt2 = points_distance_squared(rb.x, lt.y, 0.0F, 0.0F);
	float rlb2 = points_distance_squared(lt.x, rb.y, 0.0F, 0.0F);
	float rrb2 = points_distance_squared(rb.x, rb.y, 0.0F, 0.0F);

	return flsqrt(flmax(flmax(rlt2, rrt2), flmax(rlb2, rrb2)));
}

float WarGrey::DTPM::vessel_radius(float2& lt, float2& rb, double2& gps) {
	float gx = float(gps.x);
	float gy = float(gps.y);
	float rlt2 = points_distance_squared(lt.x, lt.y, gx, gy);
	float rrt2 = points_distance_squared(rb.x, lt.y, gx, gy);
	float rlb2 = points_distance_squared(lt.x, rb.y, gx, gy);
	float rrb2 = points_distance_squared(rb.x, rb.y, gx, gy);

	return flsqrt(flmax(flmax(rlt2, rrt2), flmax(rlb2, rrb2)));
}
