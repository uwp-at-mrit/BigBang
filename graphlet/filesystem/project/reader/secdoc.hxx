#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private struct SectionDot {
	public:
		SectionDot(double x, double y, double depth);
		SectionDot(double x, double y, double depth, double slope_depth, double grade, double position_sign);

	public:
		double x;
		double y;
		double depth;
		double slope_depth;
		double grade;

	public:
		double position_sign;
	};

	private ref class SecDoc sealed : public WarGrey::DTPM::ProjectDocument {
	internal:
		SecDoc(std::filebuf& sec);

	internal:
		std::deque<WarGrey::DTPM::SectionDot> centerline;
		std::deque<std::deque<WarGrey::DTPM::SectionDot>> sidelines;
	};

	/*********************************************************************************************/
	private struct ProfileDot {
		double x;
		double y;
		double distance;
		double depth;
	};

	private struct Outline {
	public:
		~Outline() noexcept;
		Outline(int side_count, int slope_count);
		Outline(const Outline* src);

	public:
		void clone_from(const Outline* src);

	public:
		WarGrey::SCADA::double3 center_foot;
		WarGrey::SCADA::double2 center_origin;
		int side_count;
		int slope_count;

	public: // for output
		WarGrey::DTPM::ProfileDot* dots;

	public: // for input
		WarGrey::DTPM::ProfileDot* sides;
		WarGrey::DTPM::ProfileDot* slopes;
	};
}
