#pragma once

Platform::String^ speak(Platform::String^ word);

template<typename E>
Platform::String^ speak(E id, Platform::String^ prefix) {
	Platform::String^ suffix = id.ToString() + ":";

	if (prefix == nullptr) {
		suffix = ":" + suffix;
	} else {
		suffix = ":" + prefix + "_" + suffix;
	}

	return speak(suffix);
}

template<typename E>
Platform::String^ speak(E id) {
	return speak(id.ToString());
}

template<typename Field>
Platform::String^ dbspeak(Field col) {
	return speak(col, "db");
}
