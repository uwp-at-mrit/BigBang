#pragma once

#include <map>
#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private struct JobLineSection {
	public:
		JobLineSection(int group, int seq,
			WarGrey::SCADA::double2& start_point, WarGrey::SCADA::double2& end_point,
			double design_depth = 0.0, Platform::String^ name = nullptr);

	public:
		int gid;
		int seq;
		double sx;
		double sy;
		double ex;
		double ey;

	public:
		double design_depth;
		Platform::String^ name;

	public:
		double angle_deg;
	};

	private ref class JobDoc sealed : public WarGrey::DTPM::ProjectDocument {
	internal:
		JobDoc(std::filebuf& job);

	internal:
		int current_job;
		int current_section;
		std::map<int, std::deque<WarGrey::DTPM::JobLineSection>> jobs;
	};
}
