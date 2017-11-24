#include "modbus/server.hpp"
#include "rsyslog.hpp"

#include <ppltasks.h>

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static inline Platform::String^ socket_identity(StreamSocket^ socket) {
    return socket->Information->RemoteHostName->RawName + ":" + socket->Information->RemotePort;
}

static void read_handle_reply_loop(ModbusServer* server, IDataReader^ mbin, IDataWriter^ mbout, StreamSocket^ socket, Platform::String^ id) {
    unsigned short header_length = (unsigned short)(2 + 2 + 2 + 1); // MMIG page 5
    create_task(mbin->LoadAsync(header_length)).then([=](unsigned int size) {
        if (size < header_length) {
            if (size == 0) {
                rsyslog(L"%s has disconnected", id->Data());
            } else {
                rsyslog(L"MBAP header from %s is too short(%u < %hu)", id->Data(), size, header_length);
            }

            cancel_current_task();
        }

        unsigned short transaction = mbin->ReadUInt16();
        unsigned short protocol = mbin->ReadUInt16();
        unsigned short length = mbin->ReadUInt16();
        unsigned char unit = mbin->ReadByte();
        unsigned short pdu_length = length - 1;

        return create_task(mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
            if (size < pdu_length) {
                rsyslog(L"PDU data from %s has been truncated(%u < %hu)", id->Data(), size, pdu_length);
                cancel_current_task();
            }

            unsigned char function_code = mbin->ReadByte();
            unsigned short data_length = (unsigned short)(mbin->UnconsumedBufferLength);

            for (size_t i = 0; i < data_length; i++) mbin->ReadByte();

            rsyslog(L"[received ADU indication(%hu, %hu, 1 + 1 + %hu, %hhu, %hhu) from %s]",
                transaction, protocol, data_length, unit, function_code, id->Data());

            mbout->WriteUInt16(transaction);
            mbout->WriteUInt16(protocol);
            mbout->WriteUInt16(4);
            mbout->WriteByte(unit);
            mbout->WriteByte(function_code);
            mbout->WriteUInt16(data_length);
        });
    }).then([=](task<void> doHandlingRequest) {
        try {
            doHandlingRequest.get();

            return create_task(mbout->StoreAsync());
        } catch (Platform::Exception^ e) {
            rsyslog(e->Message);
            cancel_current_task();
        } catch (task_canceled&) {
            rsyslog(L"Cancel dealing with request from %s", id->Data());
            cancel_current_task();
        }
    }).then([=](task<unsigned int> doReplying) {
        try {
            unsigned int sent = doReplying.get();
            rsyslog(L"[sent %u bytes to %s]", sent, id->Data());

            read_handle_reply_loop(server, mbin, mbout, socket, id);
        } catch (Platform::Exception^ e) {
            rsyslog(e->Message);
            delete socket;
        } catch (task_canceled&) {
            rsyslog(L"Cancel replying to %s", id->Data());
            delete socket;
        }
    });
}

// delegate only accepts C++/CX class
private ref class WarGrey::SCADA::ModbusListener sealed {
public:
    ModbusListener(unsigned short port) {
        this->listener = ref new StreamSocketListener();
        this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
        this->listener->Control->KeepAlive = false;
        this->port = port;
    }

    virtual ~ModbusListener() {
        delete this->listener;
    }

public:
    void welcome(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
        auto client = e->Socket;
        auto id = socket_identity(client);
        auto mbin = ref new DataReader(client->InputStream);
        auto mbout = ref new DataWriter(client->OutputStream);

        mbin->UnicodeEncoding = UnicodeEncoding::Utf8;
        mbin->ByteOrder = ByteOrder::BigEndian;
        mbout->UnicodeEncoding = UnicodeEncoding::Utf8;
        mbout->ByteOrder = ByteOrder::BigEndian;

        read_handle_reply_loop(this->server, mbin, mbout, client, id);
    }

private:
    void run(ModbusServer* server) {
        this->server = server;
        create_task(this->listener->BindEndpointAsync(nullptr, this->port.ToString())).then([this](task<void> binding) {
            try {
                binding.get();
                this->listener->ConnectionReceived += ref new TCPAcceptHandler(this, &ModbusListener::welcome);

                rsyslog(L"## 0.0.0.0:%u", this->port);
            } catch (Platform::Exception^ e) {
                rsyslog(e->Message);
            }
        });
    }

private:
    unsigned short port;
    StreamSocketListener^ listener;
    ModbusServer* server;

    friend class WarGrey::SCADA::ModbusServer;
};

/*************************************************************************************************/
ModbusServer::ModbusServer(unsigned short port) {
    this->listener = ref new ModbusListener(port);
};

ModbusServer::~ModbusServer() {
    delete this->listener;
};

ModbusServer* ModbusServer::listen() {
    this->listener->run(this);

    return this;
}
