#include "graphlet/filesystem/project/reader/digdoc.hxx"

#include "datum/flonum.hpp"
#include "datum/file.hpp"
#include "datum/box.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
DigLog::DigLog(std::filebuf& log) {
	unsigned long long max_count = read_integer(log);
	
	discard_this_line(log);

	for (unsigned int c = 0; c < max_count; c++) {
		if (peek_char(log) == EOF) {
			break;
		}

		this->digs.push_back(read_wgb18030(log, char_end_of_field));
		read_char(log);
		this->visibles.push_back(read_integer(log) == 1);
		discard_this_line(log);
	}
}

/*************************************************************************************************/
DigDoc::DigDoc(std::filebuf& dig) : lx(infinity), ty(infinity), rx(-infinity), by(-infinity) {
	IDigDatum* datum;
	
	// Lucky, 160.0F for icon size works perfectly
	while ((datum = read_dig_line(dig, 160.0F)) != nullptr) {
		if (datum->type < DigDatumType::_) {
			this->push_back_item(datum);
		}
	}

	this->cursor = this->items.end();
}

DigDoc::~DigDoc() {
	while (!this->items.empty()) {
		auto it = this->items.begin();
		
		delete (*it);
		
		this->items.erase(it);
	}
}

void DigDoc::push_back_item(WarGrey::SCADA::IDigDatum* item) {
	this->items.push_back(item);
	this->counters[item->type] = this->counters[item->type] + 1;

	this->lx = flmin(this->lx, item->lx);
	this->rx = flmax(this->rx, item->rx);
	this->ty = flmin(this->ty, item->ty);
	this->by = flmax(this->by, item->by);
}

void DigDoc::rewind() {
	this->cursor = this->items.end();
}

IDigDatum* DigDoc::step() {
	IDigDatum* datum = nullptr;

	if (this->cursor == this->items.end()) {
		this->cursor = this->items.begin();
	} else {
		this->cursor++;
	}

	if (this->cursor != this->items.end()) {
		datum = (*this->cursor);
	}

	return datum;
}

void DigDoc::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->lx, y, this->ty);
	SET_VALUES(width, this->rx - this->lx, height, this->by - this->ty);
}
