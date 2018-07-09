#pragma once

Platform::String^ speak(Platform::String^ word);

template<typename E>
Platform::String^ speak(E id) {
	return speak(id.ToString());
}

template<typename Field>
Platform::String^ db_speak(Field col) {
	return speak("db_" + col.ToString());
}
