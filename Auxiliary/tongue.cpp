#include "tongue.hpp"

#include <map>

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel::Resources;

Platform::String^ do_speak(ResourceLoader^ tongue, Platform::String^ word) {
	Platform::String^ dialect = tongue->GetString(word);

	return (dialect == nullptr) ? word : dialect;
}

Platform::String^ do_speak(Platform::String^ name, Platform::String^ word) {
	static std::map<Platform::String^, ResourceLoader^> tongues;
	auto maybe_tongue = tongues.find(name);
	ResourceLoader^ tongue = nullptr;

	if (maybe_tongue == tongues.end()) {
		tongue = ResourceLoader::GetForViewIndependentUse(name);
		tongues.insert(std::pair<Platform::String^, ResourceLoader^>(name, tongue));
	} else {
		tongue = maybe_tongue->second;
	}

	return do_speak(tongue, word);
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
ITongue::ITongue(unsigned int idx) : index(idx) {}

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

	return do_speak(this->get_type(), this->index.ToString());
}

Platform::String^ ITongue::ToLocalString() {
	return do_speak(this->get_type(), this->ToString());
}
