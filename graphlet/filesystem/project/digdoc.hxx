#pragma once

#include <deque>
#include <map>

#include "graphlet/filesystem/project/doctype.hxx"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private ref class DigLog sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		DigLog(std::filebuf& dig);

	internal:
		std::deque<Platform::String^> digs;
		std::deque<bool> visibles;
	};

	private ref class DigDoc sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		DigDoc(std::filebuf& dig);

	public:
		virtual ~DigDoc();

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height);

	internal:
		void push_back_item(WarGrey::SCADA::IDigDatum* item);
		void rewind();
		WarGrey::SCADA::IDigDatum* step();

	private:
		double lx;
		double ty;
		double rx;
		double by;

	private:
		std::deque<WarGrey::SCADA::IDigDatum*> items;
		std::deque<WarGrey::SCADA::IDigDatum*>::iterator cursor;
		std::map<WarGrey::SCADA::DigDatumType, unsigned int> counters;
	};
}
