#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"

#include "datum/file.hpp"
#include "datum/string.hpp"

using namespace WarGrey::SCADA;

static void cell_extract(ENCell* cell, bytes& permit) {
	const unsigned char* pool = permit.c_str();
	size_t psize = permit.size();
	size_t pos = 0U;

	if (psize == 64) {
		scan_bytes(pool, &pos, psize, cell->name);
		cell->expiry_year = (unsigned short)scan_natural(pool, &pos, 12, false);
		cell->expiry_month = (unsigned char)scan_natural(pool, &pos, 14, false);
		cell->expiry_day = (unsigned char)scan_natural(pool,   &pos, 16, false);
		scan_bytes(pool, &pos, psize, cell->ECK1);
		scan_bytes(pool, &pos, psize, cell->ECK2);
		scan_bytes(pool, &pos, psize, cell->checksum);
	} else {
		scan_bytes(pool, &pos, psize, cell->name);
		cell->checksum[0] = '\0';
	}
}

bool WarGrey::SCADA::ENCell::malformed() {
	return (this->checksum[0] == '\0');
}

/*************************************************************************************************/
PermitDoc::PermitDoc(std::filebuf& permit)
	: cdate(0U), chour(0U), cminute(0U), csecond(0U), version(0), content(PermitContent::_) {
	ENCell cell;
	char ch;

	while (read_char(permit) == ':') {
		std::string field = read_text(permit, char_end_of_word);

		if (field.compare("DATE") == 0) {
			this->cdate = (unsigned int)(read_natural(permit));
			this->chour = (unsigned char)(read_natural(permit));
			read_char(permit);
			this->cminute = (unsigned char)(read_natural(permit));

			if (peek_char(permit) == ':') {
				read_char(permit);
				this->csecond = (unsigned char)(read_natural(permit));
			}

			discard_this_line(permit);
		} else if (field.compare("VERSION") == 0) {
			this->version = (unsigned char)(read_natural(permit));
			discard_this_line(permit);
		} else if (field.compare("CONTENT") == 0) {
			std::string c = read_text(permit, char_end_of_line);

			if (c.compare("FULL") == 0) {
				this->content = PermitContent::Full;
			} else if (c.compare("PARTIAL") == 0) {
				this->content = PermitContent::Partial;
			}

			discard_this_line(permit);
		} else if (field.compare("ENC") == 0) {
			do {
				discard_this_line(permit);
				ch = peek_char(permit);
			
				if ((ch == ':') || (ch == EOF)) {
					break;
				}

				this->fill_cell_permit(permit, &cell);
				this->encs.push_back(cell);
			} while (1);
		} else if (field.compare("ECS") == 0) {
			do {
				discard_this_line(permit);
				ch = peek_char(permit);
				
				if ((ch == ':') || (ch == EOF)) {
					break;
				}

				this->fill_cell_permit(permit, &cell);
				this->ecss.push_back(cell);
			} while (1);
		}
	}
}

void PermitDoc::fill_cell_permit(std::filebuf& permit, ENCell* cell) {
	bytes self = read_bytes(permit, char_end_of_field);
	
	read_char(permit);
	if (read_bool(permit)) {
		cell->type = PermitServiceLevel::SinglePurchase;
	} else {
		cell->type = PermitServiceLevel::Subscription;
	}
	
	read_char(permit);
	cell->edition = read_natural(permit);
	read_char(permit);
	cell->data_server_id = read_bytes(permit, char_end_of_field);
	read_char(permit);
	cell->comment = read_wgb18030(permit, char_end_of_line);

	cell_extract(cell, self);
}
