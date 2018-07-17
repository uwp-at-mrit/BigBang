#include "tongue.hpp"

#include <map>

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel::Resources;

static ResourceLoader^ lookup_tongue(Platform::String^ name) {
	static std::map<Platform::String^, ResourceLoader^> tongues;
	auto maybe_tongue = tongues.find(name);
	ResourceLoader^ tongue = nullptr;

	if (maybe_tongue == tongues.end()) {
		tongue = ResourceLoader::GetForViewIndependentUse(name);
		tongues.insert(std::pair<Platform::String^, ResourceLoader^>(name, tongue));
	} else {
		tongue = maybe_tongue->second;
	}

	return tongue;
}

static inline Platform::String^ do_speak(ResourceLoader^ tongue, Platform::String^ word) {
	Platform::String^ dialect = tongue->GetString(word);

	return (dialect == nullptr) ? word : dialect;
}

static inline Platform::String^ do_speak(Platform::String^ name, Platform::String^ word) {
	return do_speak(lookup_tongue(name), word);
}

static inline bool do_check(Platform::String^ name, unsigned int index) {
	return (lookup_tongue(name)->GetString(index.ToString()) != nullptr);
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::speak(Platform::String^ word) {
    static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("tongue");

    return do_speak(tongue, word);
}

Platform::String^ WarGrey::SCADA::dbspeak(Platform::String^ word) {
	static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("dbtongue");

	return do_speak(tongue, word);
}

/*************************************************************************************************/
ITongue::ITongue(Platform::String^ name, unsigned int idx) : type(name), index(idx) {}

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

bool ITongue::exists(Platform::String^ name, int index) {
	return do_check(name, index);
}

int ITongue::sibling_index(Platform::String^ name, unsigned int self, int delta, unsigned int boundary) {
	int sibling = -1;
	
	if (delta < 0) {
		while ((self + delta) >= boundary) {
			self += delta;

			if (do_check(name, self)) {
				sibling = self;
				break;
			}
		}
	} else if (delta > 0) {
		while ((self + delta) <= boundary) {
			self += delta;

			if (do_check(name, self)) {
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
