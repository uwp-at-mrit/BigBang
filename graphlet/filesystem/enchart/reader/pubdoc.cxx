#include "graphlet/filesystem/enchart/reader/pubdoc.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
PublicKeyDoc::PublicKeyDoc(std::filebuf& pub) {
	Natural n;

	while (peek_char(pub) != EOF) {
		std::string comment = read_text(pub, char_end_of_word);

		if (comment.compare("//") == 0) {
			std::string big = read_text(pub, char_end_of_word);
			std::string name = read_text(pub, char_end_of_word);
			
			discard_this_line(pub);
	
			{ // read natural
				std::string hex("");

				do {
					std::string part = read_text(pub, char_end_of_word);

					if (part.size() > 0) {
						hex += part;
					} else {
						break;
					}
				} while (hex[hex.size() - 1] != '.');

				n = Natural(16U, hex, 0U, hex.size() - 1);
			}

			if (name.compare("p") == 0) {
				this->p = n;
			} else if (name.compare("q") == 0) {
				this->q = n;
			} else if (name.compare("g") == 0) {
				this->g = n;
			} else if (name.compare("y") == 0) {
				this->y = n;
			}
		}

		discard_this_line(pub);
	}
}
