#pragma once

#include <deque>

#include "graphlet/primitive.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private ref class IVesselShape abstract {};

	private class IProfileRegion abstract {
	public:
		virtual void fill_scale(double* xscale = nullptr, double* yscale = nullptr) = 0;
		virtual Windows::Foundation::Numerics::float2 vessel_to_local(double x, double y, double depth) = 0;
	};

	private class IVessellet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IVessellet();

	public:
		virtual void draw_profile(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			WarGrey::DTPM::IProfileRegion* profilet, float cx, float y, float half_width, float height) = 0;

	public:
		virtual void set_bow_direction(double degrees) = 0;
		virtual Windows::Foundation::Size original_size() = 0;

	public:
		void scale(double s);

	protected:
		void clear_boundary();
		void resolve_radius();

	protected:
		Windows::Foundation::Numerics::float2 lt;
		Windows::Foundation::Numerics::float2 rb;
		float xradius;
		float yradius;
	};

	Windows::Foundation::Numerics::float2 vessel_point(double src_x, double src_y,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2& scale, double bow_direction,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	Windows::Foundation::Numerics::float2 vessel_point(WarGrey::SCADA::double2& src,
		WarGrey::SCADA::double2& base, WarGrey::SCADA::double2& src_sign,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2& scale, double bow_direction,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	Windows::Foundation::Numerics::float2 vessel_point(WarGrey::SCADA::double2& src,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2& scale, double bow_direction,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vessel_polygon(WarGrey::SCADA::double2 src[], size_t count,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2& scale, double bow_direction,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vessel_polygon(WarGrey::SCADA::double2 src[], size_t count,
		WarGrey::SCADA::double2& base, WarGrey::SCADA::double2& src_sign,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2& scale, double bow_direction,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	float vessel_radius(Windows::Foundation::Numerics::float2& lt, Windows::Foundation::Numerics::float2& rb);
	float vessel_radius(Windows::Foundation::Numerics::float2& lt, Windows::Foundation::Numerics::float2& rb, WarGrey::SCADA::double2& gps_pos);
}
