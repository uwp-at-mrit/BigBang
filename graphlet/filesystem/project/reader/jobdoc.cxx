#include "graphlet/filesystem/project/reader/jobdoc.hxx"

#include "datum/file.hpp"
#include "datum/flonum.hpp"

#include "math.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
JobLineSection::JobLineSection(int group, double2& s, double2& e, double depth, Platform::String^ name) {
	this->id = group;
	this->sx = s.x;
	this->sy = s.y;
	this->ex = e.x;
	this->ey = e.y;
	this->design_depth = depth;
	this->name = name;

	this->angle_deg = degrees_normalize(points_angle(s.x, s.y, e.x, e.y));
	this->length = points_distance(s.x, s.y, e.x, e.y);
}

/*************************************************************************************************/
JobDoc::JobDoc(std::filebuf& job) {
	long long n = read_integer(job);
	Platform::String^ name;
	double2 spt, ept;
	double depth;

	read_char(job);
	this->default_group = int(read_integer(job));
	discard_this_line(job);

	for (long long idx = 0; idx < n; idx++) {
		if (peek_char(job) == EOF) {
			break;
		} else {
			int id = int(read_integer(job));
			
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

			this->sections[id].push_back(JobLineSection(id, spt, ept, depth, name));

			discard_this_line(job);
		}
	}
}
