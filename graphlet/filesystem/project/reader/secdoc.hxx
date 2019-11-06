#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

namespace WarGrey::SCADA {
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

	private ref class SecDoc sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		SecDoc(std::filebuf& sec);

	internal:
		std::deque<WarGrey::SCADA::SectionDot> centerline;
		std::deque<std::deque<WarGrey::SCADA::SectionDot>> sidelines;
	};
}
