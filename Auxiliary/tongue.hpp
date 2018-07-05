#pragma once

Platform::String^ speak(Platform::String^ word);

template<typename E>
Platform::String^ speak(E id) {
	return speak(id.ToString());
}
