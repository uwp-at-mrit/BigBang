#pragma once

Platform::String^ modbus_socket_strerror(Platform::Exception^ e);
Windows::Networking::Sockets::SocketErrorStatus modbus_socket_error(Platform::Exception^ e);
