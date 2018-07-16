#pragma once

namespace WarGrey::SCADA {
	Platform::String^ speak(Platform::String^ word);
	Platform::String^ dbspeak(Platform::String^ field);

	template<typename E>
	Platform::String^ speak(E id, Platform::String^ prefix) {
		Platform::String^ suffix = id.ToString() + ":";

		if (prefix == nullptr) {
			suffix = ":" + suffix;
		} else {
			suffix = ":" + prefix + "_" + suffix;
		}

		return WarGrey::SCADA::speak(suffix);
	}

	template<typename E>
	Platform::String^ speak(E id) {
		return WarGrey::SCADA::speak(id.ToString());
	}

	template<typename E>
	Platform::String^ dbspeak(E id) {
		return WarGrey::SCADA::dbspeak(":" + id.ToString() + ":");
	}

	private class ITongue abstract {
	public:
		ITongue(unsigned int index);

	public:
		unsigned int ToIndex();
		Platform::String^ ToString(); // return the identity of the instance, analogue to enum.ToString();
		Platform::String^ ToLocalString();

	protected:
		virtual Platform::String^ get_type() = 0;

	protected:
		int unsafe_compare(ITongue* instance);

	private:
		unsigned int index;
	};

	template<typename E>
	private class Tongue abstract : public WarGrey::SCADA::ITongue {
	public:
		Tongue(unsigned int idx) : ITongue(idx) {}

	public:
		bool eq(E* instance) { return (this->unsafe_compare(instance) == 0); }
		bool lt(E* instance) { return (this->unsafe_compare(instance) <  0); }
		bool le(E* instance) { return (this->unsafe_compare(instance) <= 0); }
		bool gt(E* instance) { return (this->unsafe_compare(instance) >  0); }
		bool ge(E* instance) { return (this->unsafe_compare(instance) >= 0); }
	};
}
