#include "sockexn.hpp"

using namespace Windows::Networking::Sockets;

Platform::String^ modbus_socket_strerror(Platform::Exception^ e) {
	return modbus_socket_error(e).ToString();
}

SocketErrorStatus modbus_socket_error(Platform::Exception^ e) {
	return SocketError::GetStatus(e->HResult);
}
