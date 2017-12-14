#include <ppltasks.h>

#include "modbus/client.hpp"
#include "modbus/protocol.hpp"
#include "modbus/sockexn.hpp"
#include "modbus/adu.hpp"
#include "rsyslog.hpp"

// MMIG: Page 20

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

#define POP_REQUEST(self) auto it = self->begin(); free(it->second.pdu_data); self->erase(it);

private struct WarGrey::SCADA::ModbusTransaction {
	uint8 function_code;
	uint8* pdu_data;
	uint16 size;
	IModbusConfirmation* confirmation;
};

static inline uint16 modbus_simple_request(IModbusClient* self, uint8 fcode, uint16 addr, uint16 val, IModbusConfirmation* cb) {
	uint8* pdu_data = self->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, addr);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, val);

	return self->request(fcode, pdu_data, 4, cb);
}

static inline uint16 modbus_write_register_request(IModbusClient* self, uint8* pdu_data, uint8 offset
	, uint8 fcode, uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* cb) {
	uint8* data = pdu_data + offset;
	uint8 NStar = MODBUS_REGISTER_NStar(quantity);

	MODBUS_SET_INT16_TO_INT8(data, 0, address);
	MODBUS_SET_INT16_TO_INT8(data, 2, quantity);
	data[4] = NStar;
	modbus_read_registers(src, 0, NStar, data + 5);

	return self->request(fcode, pdu_data, offset + 5 + NStar, cb);
}

static void modbus_apply_positive_confirmation(IModbusConfirmation* cb, uint16 transaction, uint8 function_code, DataReader^ mbin) {
	switch (function_code) {
	case MODBUS_READ_COILS: case MODBUS_READ_DISCRETE_INPUTS: {               // MAP: Page 11, 12
		static uint8 status[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte();
		MODBUS_READ_BYTES(mbin, status, count);

		if (function_code == MODBUS_READ_COILS) {
			cb->on_coils(transaction, status, count);
		} else {
			cb->on_discrete_inputs(transaction, status, count);
		}
	} break;
	case MODBUS_READ_HOLDING_REGISTERS: case MODBUS_READ_INPUT_REGISTERS:     // MAP: Page 15, 16
	case MODBUS_WRITE_AND_READ_REGISTERS: {                                   // MAP: Page 38
		static uint16 registers[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte() / 2;
		MODBUS_READ_DOUBLES(mbin, registers, count);

		if (function_code == MODBUS_READ_INPUT_REGISTERS) {
			cb->on_input_registers(transaction, registers, count);
		} else {
			cb->on_holding_registers(transaction, registers, count);
		}
	} break;
	case MODBUS_WRITE_SINGLE_COIL: case MODBUS_WRITE_SINGLE_REGISTER:         // MAP: Page 17, 19
	case MODBUS_WRITE_MULTIPLE_COILS: case MODBUS_WRITE_MULTIPLE_REGISTERS: { // MAP: Page 29, 30
		uint16 address = mbin->ReadUInt16();
		uint16 value = mbin->ReadUInt16();

		cb->on_echo_response(transaction, function_code, address, value);
	} break;
	case MODBUS_MASK_WRITE_REGISTER: {                                        // MAP: Page 36
		uint16 address = mbin->ReadUInt16();
		uint16 and_mask = mbin->ReadUInt16();
		uint16 or_mask = mbin->ReadUInt16();

		cb->on_echo_response(transaction, function_code, address, and_mask, or_mask);
	} break;
	case MODBUS_READ_FIFO_QUEUES: {                                           // MAP: Page 40
		static uint16 queues[MODBUS_MAX_PDU_LENGTH];
		uint16 useless = mbin->ReadUInt16();
		uint16 count = mbin->ReadUInt16();

		cb->on_queue_registers(transaction, queues, count);
	} break;
	default: {
		static uint8 raw_data[MODBUS_MAX_PDU_LENGTH];
		static uint8 count = (uint8)mbin->UnconsumedBufferLength;

		MODBUS_READ_BYTES(mbin, raw_data, count);
		cb->on_private_response(transaction, function_code, raw_data, count);
	}
	}
}

/*************************************************************************************************/
IModbusClient::IModbusClient(Platform::String^ server, uint16 port, IModbusTransactionIdGenerator* generator) {
    this->device = ref new HostName(server);
    this->service = port.ToString();

	this->blocking_requests = new std::map<uint16, ModbusTransaction>();
	this->pending_requests = new std::map<uint16, ModbusTransaction>();
	this->pdu_pool = new std::queue<uint8*>();

	this->generator = generator;
	this->generator->reference();

    this->connect();
};

IModbusClient::~IModbusClient() {
	this->generator->destroy();

	while (!this->blocking_requests->empty()) {
		POP_REQUEST(this->blocking_requests);
	}

	while (!this->pending_requests->empty()) {
		POP_REQUEST(this->pending_requests);
	}

	while (!this->pdu_pool->empty()) {
		free(this->pdu_pool->front());
		this->pdu_pool->pop();
	}

	delete this->blocking_requests;
	delete this->pending_requests;
	delete this->pdu_pool;
}

void IModbusClient::connect() {
	if (this->mbout != nullptr) {
		delete this->socket;
		delete this->mbin;
		delete this->mbout;

		this->mbout = nullptr;
	}

	this->socket = ref new StreamSocket();
	this->socket->Control->KeepAlive = false;

    create_task(this->socket->ConnectAsync(this->device, this->service)).then([this](task<void> handshaking) {
        try {
            handshaking.get();

            this->mbin  = ref new DataReader(this->socket->InputStream);
            this->mbout = ref new DataWriter(this->socket->OutputStream);

            mbin->UnicodeEncoding = UnicodeEncoding::Utf8;
            mbin->ByteOrder = ByteOrder::BigEndian;
            mbout->UnicodeEncoding = UnicodeEncoding::Utf8;
            mbout->ByteOrder = ByteOrder::BigEndian;

            rsyslog(L">> connected to %s:%s", this->device->RawName->Data(), this->service->Data());

			this->wait_process_callback_loop();

			while (!this->blocking_requests->empty()) {
				auto current = this->blocking_requests->begin();

				this->apply_request(std::pair<uint16, ModbusTransaction>(current->first, current->second));
				this->blocking_requests->erase(current);
			};
        } catch (Platform::Exception^ e) {
			this->connect();
        }
    });
}

uint8* IModbusClient::calloc_pdu() {
	if (this->pdu_pool->empty()) {
		this->pdu_pool->push((uint8*)calloc(MODBUS_MAX_PDU_LENGTH, sizeof(uint8)));
	}

	auto head = this->pdu_pool->front();
	this->pdu_pool->pop();

	return head;
}

uint16 IModbusClient::request(uint8 function_code, uint8* data, uint16 size, IModbusConfirmation* confirmation) {
	ModbusTransaction mt = { function_code, data, size, confirmation };
	uint16 id = this->generator->yield();
	auto transaction = std::pair<uint16, ModbusTransaction>(id, mt);

	if (this->mbout == nullptr) {
		this->blocking_requests->insert(transaction);
	} else {
		this->apply_request(transaction);
	}

	return id;
}

void IModbusClient::apply_request(std::pair<uint16, ModbusTransaction>& transaction) {
	uint16 tid = transaction.first;
	uint8 fcode = transaction.second.function_code;

	modbus_write_adu(this->mbout, tid, 0x00, 0xFF, fcode, transaction.second.pdu_data, transaction.second.size);

	this->pending_requests->insert(transaction);
	create_task(this->mbout->StoreAsync()).then([=](task<unsigned int> sending) {
		try {
			unsigned int sent = sending.get();

			if (this->debug) {
				rsyslog(L"<sent %u-byte-request for function 0x%02X as transaction %hu to %s:%s>",
					sent, fcode, tid, this->device->RawName->Data(), this->service->Data());
			}
		} catch (task_canceled&) {
		} catch (Platform::Exception^ e) {
			rsyslog(e->Message);
			this->blocking_requests->insert(transaction);
			this->connect();
		}});
}

void IModbusClient::wait_process_callback_loop() {
	create_task(this->mbin->LoadAsync(MODBUS_MBAP_LENGTH)).then([=](unsigned int size) {
		uint16 transaction, protocol, length;
		uint8 unit;

		if (size < MODBUS_MBAP_LENGTH) {
			if (size == 0) {
				modbus_protocol_fatal(L"Server %s:%s has lost", this->device->RawName->Data(), this->service->Data());
			} else {
				modbus_protocol_fatal(L"MBAP header comes from server %s:%s is too short(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, MODBUS_MBAP_LENGTH);
			}
		}

		uint16 pdu_length = modbus_read_mbap(mbin, &transaction, &protocol, &length, &unit);

		return create_task(mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
			if (size < pdu_length) {
				modbus_protocol_fatal(L"PDU data comes from server %s:%s has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, pdu_length);
			}

			auto maybe_transaction = this->pending_requests->find(transaction);
			if (maybe_transaction == this->pending_requests->end()) {
				modbus_discard_current_adu(this->debug,
					L"<discarded non-pending confirmation(%hu) comes from %s:%s>",
					transaction, this->device->RawName->Data(), this->service->Data());
			} else {
				this->pdu_pool->push(maybe_transaction->second.pdu_data);
				this->pending_requests->erase(maybe_transaction);
			}

			if ((protocol != MODBUS_PROTOCOL) || (unit != MODBUS_TCP_SLAVE)) {
				modbus_discard_current_adu(this->debug,
					L"<discarded non-modbus-tcp confirmation(%hu, %hu, %hu, %hhu) comes from %s:%s>",
					transaction, protocol, length, unit, this->device->RawName->Data(), this->service->Data());
			}

			IModbusConfirmation* cb = maybe_transaction->second.confirmation;
			uint8 origin_code = maybe_transaction->second.function_code;
			uint8 raw_code = mbin->ReadByte();
			uint8 function_code = (raw_code > 0x80) ? (raw_code - 0x80) : raw_code;

			if (function_code != origin_code) {
				modbus_discard_current_adu(this->debug,
					L"<discarded negative confirmation due to non-expected function(0x%02X) comes from %s:%s>",
					function_code, this->device->RawName->Data(), this->service->Data());
			} else if (this->debug) {
				rsyslog(L"<received confirmation(%hu, %hu, %hu, %hhu) for function 0x%02X comes from %s:%s>",
					transaction, protocol, length, unit, function_code,
					this->device->RawName->Data(), this->service->Data());
			}

			if (cb != nullptr) {
				if (function_code != raw_code) {
					cb->on_exception(transaction, function_code, this->mbin->ReadByte());
				} else {
					modbus_apply_positive_confirmation(cb, transaction, function_code, this->mbin);
				}
			}
		});
	}).then([=](task<void> confirm) {
		try {
			confirm.get();

			unsigned int dirty = mbin->UnconsumedBufferLength;

			if (dirty > 0) {
				MODBUS_DISCARD_BYTES(mbin, dirty);
				if (this->debug) {
					rsyslog(L"<discarded last %u bytes of the confirmation comes from %s:%s>",
						dirty, this->device->RawName->Data(), this->service->Data());
				}
			}

			this->wait_process_callback_loop();
		} catch (modbus_adu_discarded&) {
			unsigned int rest = mbin->UnconsumedBufferLength;
			
			if (rest > 0) {
				MODBUS_DISCARD_BYTES(mbin, rest);
			}
			
			this->wait_process_callback_loop();
		} catch (task_canceled&) {
			this->connect();
		} catch (Platform::Exception^ e) {
			rsyslog(e->Message);
			this->connect();
		}
	});
}

void IModbusClient::enable_debug(bool on_or_off) {
	this->debug = on_or_off;
}

bool IModbusClient::debug_enabled() {
	return this->debug;
}

/*************************************************************************************************/
uint16 ModbusClient::read_coils(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) { // MAP: Page 11
	return modbus_simple_request(this, MODBUS_READ_COILS, address, quantity, confirmation);
}

uint16 ModbusClient::read_discrete_inputs(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) { // MAP: Page 12
	return modbus_simple_request(this, MODBUS_READ_DISCRETE_INPUTS, address, quantity, confirmation);
}

uint16 ModbusClient::write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) { // MAP: Page 17
	return modbus_simple_request(this, MODBUS_WRITE_SINGLE_COIL, address, (value ? 0xFF00 : 0x0000), confirmation);
}

uint16 ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* src, IModbusConfirmation* confirmation) { // MAP: Page 29
	uint8* pdu_data = this->calloc_pdu();
	uint8 NStar = MODBUS_COIL_NStar(quantity);

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, quantity);
	pdu_data[4] = NStar;
	memcpy(pdu_data + 5, src, NStar);
	
	return IModbusClient::request(MODBUS_WRITE_MULTIPLE_COILS, pdu_data, 5 + NStar, confirmation);
}

uint16 ModbusClient::read_holding_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) {
	return modbus_simple_request(this, MODBUS_READ_HOLDING_REGISTERS, address, quantity, confirmation);
}

uint16 ModbusClient::read_input_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) {
	return modbus_simple_request(this, MODBUS_READ_INPUT_REGISTERS, address, quantity, confirmation);
}

uint16 ModbusClient::read_queues(uint16 address, IModbusConfirmation* confirmation) {
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);

	return IModbusClient::request(MODBUS_READ_FIFO_QUEUES, pdu_data, 2, confirmation);
}


uint16 ModbusClient::write_register(uint16 address, uint16 value, IModbusConfirmation* confirmation) { // MAP: Page 19
	return modbus_simple_request(this, MODBUS_WRITE_SINGLE_REGISTER, address, value, confirmation);
}

uint16 ModbusClient::write_registers(uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* confirmation) { // MAP: Page 30
	uint8* pdu_data = this->calloc_pdu();

	return modbus_write_register_request(this, pdu_data, 0, MODBUS_WRITE_MULTIPLE_REGISTERS, address, quantity, src, confirmation);
}

uint16 ModbusClient::mask_write_register(uint16 address, uint16 and, uint16 or, IModbusConfirmation* confirmation) { // MAP: Page 36
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, and);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 4, or);

	return IModbusClient::request(MODBUS_MASK_WRITE_REGISTER, pdu_data, 6, confirmation);
}

uint16 ModbusClient::write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src, IModbusConfirmation* confirmation) {
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, raddr);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, rquantity);

	return modbus_write_register_request(this, pdu_data, 4,
		MODBUS_WRITE_AND_READ_REGISTERS, waddr, wquantity, src, confirmation);
}

uint16 ModbusClient::do_private_function(uint8 fcode, uint8* request, uint16 data_length, IModbusConfirmation* confirmation) {
	return IModbusClient::request(fcode, request, data_length, confirmation);
}

/*************************************************************************************************/
ModbusSequenceGenerator::ModbusSequenceGenerator(uint16 start) {
	this->start = ((start == 0) ? 1 : start);
	this->sequence = this->start;
}

void ModbusSequenceGenerator::reset() {
	this->sequence = this->start;
}

uint16 ModbusSequenceGenerator::yield() {
	uint16 seq = this->sequence;

	if (this->sequence == 0xFFFF) {
		this->reset();
	} else {
		this->sequence++;
	}

	return seq;
}
