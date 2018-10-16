#include <map>

#include "tongue.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel::Resources;

static ResourceLoader^ lookup_tongue(Platform::String^ scope) {
	static std::map<Platform::String^, ResourceLoader^> tongues;
	auto maybe_tongue = tongues.find(scope);
	ResourceLoader^ tongue = nullptr;

	if (maybe_tongue == tongues.end()) {
		tongue = ResourceLoader::GetForViewIndependentUse(scope);
		tongues.insert(std::pair<Platform::String^, ResourceLoader^>(scope, tongue));
	} else {
		tongue = maybe_tongue->second;
	}

	return tongue;
}

static inline Platform::String^ do_speak(ResourceLoader^ tongue, Platform::String^ word) {
	Platform::String^ dialect = ((word == nullptr) ? word : tongue->GetString(word));

	return (dialect == nullptr) ? word : dialect;
}

static inline Platform::String^ do_speak(ResourceLoader^ tongue, Platform::String^ word, bool* exists) {
	Platform::String^ dialect = ((word == nullptr) ? word : tongue->GetString(word));

	if (exists != nullptr) {
		(*exists) = (dialect != nullptr);
	}

	return (dialect == nullptr) ? word : dialect;
}

static inline Platform::String^ do_speak(Platform::String^ scope, Platform::String^ word) {
	return do_speak(lookup_tongue(scope), word);
}

static inline Platform::String^ do_speak(Platform::String^ scope, Platform::String^ word, bool* exists) {
	return do_speak(lookup_tongue(scope), word, exists);
}

static inline bool do_check(Platform::String^ scope, Platform::String^ word) {
	return (lookup_tongue(scope)->GetString(word) != nullptr);
}

static inline bool do_check(Platform::String^ scope, unsigned int index) {
	return do_check(scope, index.ToString());
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::speak(Platform::String^ word) {
    static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("tongue");

    return do_speak(tongue, word);
}

Platform::String^ WarGrey::SCADA::speak(Platform::String^ word, Platform::String^ scope) {
	return ((scope == nullptr) ? speak(word) : do_speak(scope, word));
}

Platform::String^ WarGrey::SCADA::speak(Platform::String^ word, Platform::String^ scope, bool* exists) {
	return do_speak(scope, word, exists);
}

Platform::String^ WarGrey::SCADA::dbspeak(Platform::String^ word) {
	static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("dbfield");

	return do_speak(tongue, word);
}

Platform::String^ WarGrey::SCADA::unitspeak(Platform::String^ unit) {
	bool exists;
	Platform::String^ dialect = speak(unit, "unit", &exists);

	return (exists ? dialect : speak(unit));
}

/*************************************************************************************************/
ITongue::ITongue(Platform::String^ scope, unsigned int idx) : type(scope), index(idx) {}

int ITongue::unsafe_compare(ITongue* instance) {
	unsigned int sidx = this->ToIndex();
	unsigned int tidx = instance->ToIndex();
	int sign = 0;

	if (sidx < tidx) {
		sign = -1;
	} else if (sidx > tidx) {
		sign = 1;
	}

	return sign;
}

bool ITongue::exists(Platform::String^ scope, int index) {
	return do_check(scope, index);
}

int ITongue::sibling_index(Platform::String^ scope, unsigned int self, int delta, unsigned int boundary) {
	ResourceLoader^ tongue = lookup_tongue(scope);
	int sibling = -1;
	
	if (delta < 0) {
		while ((self + delta) >= boundary) {
			self += delta;

			if (tongue->GetString(self.ToString()) != nullptr) {
				sibling = self;
				break;
			}
		}
	} else if (delta > 0) {
		while ((self + delta) <= boundary) {
			self += delta;

			if (tongue->GetString(self.ToString()) != nullptr) {
				sibling = self;
				break;
			}
		}
	}

	return sibling;
}

unsigned int ITongue::ToIndex() {
	return this->index;
}

Platform::String^ ITongue::ToString() {
	/**	
	 * By default, `ITongue` is designed for database localization.
	 *   All strings are stored as integers in the database, and these integers may not continuous,
	 *   Therefore here we have the name of the integer, and all other localized strings are
	 *   looked up by names. 
	 *
	 * NOTE: This is not an elegant way,
	 *   since UWP does not have an independent place to store string resources that do not need to be localized,
	 *   such strings have to be stored in the default language context.
	 */

	return do_speak(this->type, this->index.ToString());
}

Platform::String^ ITongue::ToLocalString() {
	return do_speak(this->type, this->ToString());
}
