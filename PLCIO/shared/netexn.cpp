#include "netexn.hpp"

using namespace Windows::Networking::Sockets;

Platform::String^ socket_strerror(Platform::Exception^ e) {
	return socket_error(e).ToString();
}

SocketErrorStatus socket_error(Platform::Exception^ e) {
	return SocketError::GetStatus(e->HResult);
}
