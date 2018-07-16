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

	private class Tongue abstract {
	public:
		Tongue(unsigned int value, Platform::String^ id, Platform::String^ en_US, Platform::String^ zh_CN)
			: value(value), id(id), en_US(en_US), zh_CN(zh_CN) {}

	public:
		unsigned int ToIndex() {
			return this->value;
		}

		Platform::String^ ToString() {
			return this->id;
		}

	protected:
		unsigned int value;
		Platform::String^ id;
		Platform::String^ en_US;
		Platform::String^ zh_CN;
	};
}
