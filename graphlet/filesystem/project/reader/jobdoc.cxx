#include "graphlet/filesystem/project/reader/jobdoc.hxx"

#include "datum/file.hpp"
#include "datum/flonum.hpp"

#include "math.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
JobLineSection::JobLineSection(int group, int seq, double2& s, double2& e, double depth, Platform::String^ name) {
	this->gid = group;
	this->seq = seq;

	this->sx = s.x;
	this->sy = s.y;
	this->ex = e.x;
	this->ey = e.y;
	this->design_depth = depth;
	this->name = name;

	this->angle_deg = degrees_normalize(points_angle(s.x, s.y, e.x, e.y));
	this->length = points_distance(this->sx, this->sy, this->ex, this->ey);
}

/*************************************************************************************************/
JobDoc::JobDoc(std::filebuf& job) {
	long long n = read_integer(job);
	Platform::String^ name;
	double2 spt, ept;
	double depth;

	read_char(job);
	this->current_job = int(read_integer(job));
	this->current_section = -1;
	discard_this_line(job);

	for (long long idx = 0; idx < n; idx++) {
		if (peek_char(job) == EOF) {
			break;
		} else {
			int gid = int(read_integer(job));
			int seq = int(this->jobs[gid].size());
			
			read_char(job);
			spt.x = read_flonum(job);
			read_char(job);
			spt.y = read_flonum(job);
			read_char(job);
			ept.x = read_flonum(job);
			read_char(job);
			ept.y = read_flonum(job);
			read_char(job);
			depth = read_flonum(job);
			read_char(job);
			name = read_wgb18030(job, char_end_of_field);

			this->jobs[gid].push_back(JobLineSection(gid, seq, spt, ept, depth, name));

			discard_this_line(job);
		}
	}
}
