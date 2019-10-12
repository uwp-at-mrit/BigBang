#pragma once

#include "graphlet/primitive.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private ref class IVessel abstract {};

	private class IVessellet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IVessellet();

	public:
		virtual Windows::Foundation::Size original_size() = 0;
	};

	Windows::Foundation::Numerics::float2 vessel_point(WarGrey::SCADA::double2 src,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2 scale,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vessel_polygon(WarGrey::SCADA::double2 src[], size_t count,
		WarGrey::SCADA::double2& gps_pos, Windows::Foundation::Numerics::float2 scale,
		Windows::Foundation::Numerics::float2* lt = nullptr, Windows::Foundation::Numerics::float2* rb = nullptr);

	float vessel_radius(Windows::Foundation::Numerics::float2& lt, Windows::Foundation::Numerics::float2& rb);
	float vessel_radius(Windows::Foundation::Numerics::float2& lt, Windows::Foundation::Numerics::float2& rb, WarGrey::SCADA::double2& gps_pos);
}
