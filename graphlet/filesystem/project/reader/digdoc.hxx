#pragma once

#include <deque>
#include <map>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::DTPM {
	private ref class DigDoc sealed : public WarGrey::DTPM::ProjectDocument {
	internal:
		DigDoc(std::filebuf& dig);

	public:
		virtual ~DigDoc();

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height);

	internal:
		void push_back_item(WarGrey::DTPM::IDigDatum* item);
		void rewind();
		WarGrey::DTPM::IDigDatum* step();

	private:
		double lx;
		double ty;
		double rx;
		double by;

	private:
		std::deque<WarGrey::DTPM::IDigDatum*> items;
		std::deque<WarGrey::DTPM::IDigDatum*>::iterator cursor;
		std::map<WarGrey::DTPM::DigDatumType, unsigned int> counters;
	};
}
