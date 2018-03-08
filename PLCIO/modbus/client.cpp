#include <ppltasks.h>

#include "modbus/client.hpp"
#include "modbus/protocol.hpp"
#include "modbus/sockexn.hpp"
#include "modbus/exception.hpp"
#include "syslog.hpp"
#include "box.hpp"

// MMIG: Page 20

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

#define POP_REQUEST(self) auto it = self.begin(); free(it->second.pdu_data); self.erase(it);

private struct WarGrey::SCADA::ModbusTransaction {
	uint8 function_code;
	uint8* pdu_data;
	uint16 size;
	uint16 maybe_address;
	IModbusConfirmation* confirmation;
};

static void modbus_apply_positive_confirmation(IModbusConfirmation* cf, Syslog* logger, DataReader^ mbin
	, uint16 transaction, uint8 function_code, uint16 address) {
	switch (function_code) {
	case MODBUS_READ_COILS: case MODBUS_READ_DISCRETE_INPUTS: {               // MAP: Page 11, 12
		static uint8 status[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte();
		MODBUS_READ_BYTES(mbin, status, count);

		if (function_code == MODBUS_READ_COILS) {
			cf->on_coils(transaction, address, status, count, logger);
		} else {
			cf->on_discrete_inputs(transaction, address, status, count, logger);
		}
	} break;
	case MODBUS_READ_HOLDING_REGISTERS: case MODBUS_READ_INPUT_REGISTERS:     // MAP: Page 15, 16
	case MODBUS_WRITE_AND_READ_REGISTERS: {                                   // MAP: Page 38
		static uint16 registers[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte() / 2;
		MODBUS_READ_DOUBLES(mbin, registers, count);

		if (function_code == MODBUS_READ_INPUT_REGISTERS) {
			cf->on_input_registers(transaction, address, registers, count, logger);
		} else {
			cf->on_holding_registers(transaction, address, registers, count, logger);
		}
	} break;
	case MODBUS_WRITE_SINGLE_COIL: case MODBUS_WRITE_SINGLE_REGISTER:         // MAP: Page 17, 19
	case MODBUS_WRITE_MULTIPLE_COILS: case MODBUS_WRITE_MULTIPLE_REGISTERS: { // MAP: Page 29, 30
		uint16 address = mbin->ReadUInt16();
		uint16 value = mbin->ReadUInt16();

		cf->on_echo_response(transaction, function_code, address, value, logger);
	} break;
	case MODBUS_MASK_WRITE_REGISTER: {                                        // MAP: Page 36
		uint16 address = mbin->ReadUInt16();
		uint16 and_mask = mbin->ReadUInt16();
		uint16 or_mask = mbin->ReadUInt16();

		cf->on_echo_response(transaction, function_code, address, and_mask, or_mask, logger);
	} break;
	case MODBUS_READ_FIFO_QUEUES: {                                           // MAP: Page 40
		static uint16 queues[MODBUS_MAX_PDU_LENGTH];
		uint16 useless = mbin->ReadUInt16();
		uint16 count = mbin->ReadUInt16();

		cf->on_queue_registers(transaction, address, queues, count, logger);
	} break;
	default: {
		static uint8 raw_data[MODBUS_MAX_PDU_LENGTH];
		static uint8 count = (uint8)mbin->UnconsumedBufferLength;

		MODBUS_READ_BYTES(mbin, raw_data, count);
		cf->on_private_response(transaction, function_code, raw_data, count, logger);
	}
	}
}

/*************************************************************************************************/
IModbusClient::IModbusClient(Syslog* sl, Platform::String^ h, uint16 p, IModbusConfirmation* cf, IModbusTransactionIdGenerator* g) {
	this->logger = ((sl == nullptr) ? new Syslog(Log::None, "Silent Modbus Client", nullptr) : sl);
	this->logger->reference();

	this->device = ref new HostName(h);
    this->service = p.ToString();

	this->confirmation = cf;
	this->generator = ((g == nullptr) ? new WarGrey::SCADA::ModbusSequenceGenerator() : g);
	this->generator->reference();

    this->connect();
};

IModbusClient::~IModbusClient() {
	this->logger->destroy();
	this->generator->destroy();

	delete this->socket; // stop the confirmation loop before release PDU pool.

	this->blocking_section.lock();
	while (!this->blocking_requests.empty()) {
		POP_REQUEST(this->blocking_requests);
	}
	this->blocking_section.unlock();

	this->pending_section.lock();
	while (!this->pending_requests.empty()) {
		POP_REQUEST(this->pending_requests);
	}
	this->pending_section.unlock();

	this->pdu_section.lock();
	while (!this->pdu_pool.empty()) {
		free(this->pdu_pool.front());
		this->pdu_pool.pop();
	}
	this->pdu_section.unlock();
}

Platform::String^ IModbusClient::device_hostname() {
	return this->device->RawName;
}

Syslog* IModbusClient::get_logger() {
	return this->logger;
}

void IModbusClient::send_scheduled_request(long long count, long long interval, long long uptime, bool is_slow) {
	if (this->confirmation != nullptr) {
		this->confirmation->on_scheduled_request(this, count, interval, uptime, is_slow);
	}
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

            this->logger->log_message(Log::Info, L">> connected to %s:%s", this->device->RawName->Data(), this->service->Data());

			this->wait_process_confirm_loop();

			this->blocking_section.lock();
			while (!this->blocking_requests.empty()) {
				auto current = this->blocking_requests.begin();

				this->apply_request(std::pair<uint16, ModbusTransaction>(current->first, current->second));
				this->blocking_requests.erase(current);
			}
			this->blocking_section.unlock();
        } catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, modbus_socket_strerror(e));
			this->connect();
        }
    });
}

uint8* IModbusClient::calloc_pdu() {
	uint8* pdu = nullptr;

	this->pdu_section.lock();
	if (this->pdu_pool.empty()) {
		pdu = (uint8*)calloc(MODBUS_MAX_PDU_LENGTH, sizeof(uint8));
	} else {
		pdu = this->pdu_pool.front();
		this->pdu_pool.pop();
	}
	this->pdu_section.unlock();

	return pdu;
}

uint16 IModbusClient::request(uint8 function_code, uint8* data, uint16 size, IModbusConfirmation* alt_confirmation) {
	IModbusConfirmation* cf = ((alt_confirmation == nullptr) ? this->confirmation : alt_confirmation);
	uint16 maybe_address = MODBUS_GET_INT16_FROM_INT8(data, 0);
	ModbusTransaction mt = { function_code, data, size, maybe_address, cf };
	uint16 id = this->generator->yield();
	auto transaction = std::pair<uint16, ModbusTransaction>(id, mt);

	if (this->mbout == nullptr) {
		this->blocking_section.lock();
		this->blocking_requests.insert(transaction);
		this->blocking_section.unlock();
	} else {
		this->apply_request(transaction);
	}

	return id;
}

void IModbusClient::apply_request(std::pair<uint16, ModbusTransaction>& transaction) {
	uint16 tid = transaction.first;
	uint8 fcode = transaction.second.function_code;

	modbus_write_adu(this->mbout, tid, 0x00, 0xFF, fcode, transaction.second.pdu_data, transaction.second.size);

	this->pending_section.lock();
	this->pending_requests.insert(transaction);
	this->pending_section.unlock();

	create_task(this->mbout->StoreAsync()).then([=](task<unsigned int> sending) {
		try {
			unsigned int sent = sending.get();

			this->logger->log_message(Log::Debug,
				L"<sent %u-byte-request for function 0x%02X as transaction %hu to %s:%s>",
				sent, fcode, tid, this->device->RawName->Data(), this->service->Data());
		} catch (task_canceled&) {
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->connect();
		}});
}

void IModbusClient::wait_process_confirm_loop() {
	create_task(this->mbin->LoadAsync(MODBUS_MBAP_LENGTH)).then([=](unsigned int size) {
		uint16 transaction, protocol, length;
		uint8 unit;

		if (size < MODBUS_MBAP_LENGTH) {
			if (size == 0) {
				modbus_protocol_fatal(this->logger,
					L"Server %s:%s has lost",
					this->device->RawName->Data(), this->service->Data());
			} else {
				modbus_protocol_fatal(this->logger,
					L"MBAP header comes from server %s:%s is too short(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, MODBUS_MBAP_LENGTH);
			}
		}

		uint16 pdu_length = modbus_read_mbap(mbin, &transaction, &protocol, &length, &unit);

		return create_task(mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
			if (size < pdu_length) {
				modbus_protocol_fatal(this->logger,
					L"PDU data comes from server %s:%s has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, pdu_length);
			}

			this->pending_section.lock();
			auto maybe_transaction = this->pending_requests.find(transaction);
			if (maybe_transaction == this->pending_requests.end()) {
				this->pending_section.unlock();
				modbus_discard_current_adu(this->logger,
					L"<discarded non-pending confirmation(%hu) comes from %s:%s>",
					transaction, this->device->RawName->Data(), this->service->Data());
			} else {
				this->pdu_section.lock();
				this->pdu_pool.push(maybe_transaction->second.pdu_data);
				this->pdu_section.unlock();
				this->pending_requests.erase(maybe_transaction);
			}
			this->pending_section.unlock();

			if ((protocol != MODBUS_PROTOCOL) || (unit != MODBUS_TCP_SLAVE)) {
				modbus_discard_current_adu(this->logger,
					L"<discarded non-modbus-tcp confirmation(%hu, %hu, %hu, %hhu) comes from %s:%s>",
					transaction, protocol, length, unit, this->device->RawName->Data(), this->service->Data());
			}

			IModbusConfirmation* cf = maybe_transaction->second.confirmation;
			uint16 maybe_address = maybe_transaction->second.maybe_address;
			uint8 origin_code = maybe_transaction->second.function_code;
			uint8 raw_code = mbin->ReadByte();
			uint8 function_code = ((raw_code > 0x80) ? (raw_code - 0x80) : raw_code);

			if (function_code != origin_code) {
				modbus_discard_current_adu(this->logger,
					L"<discarded negative confirmation due to non-expected function(0x%02X) comes from %s:%s>",
					function_code, this->device->RawName->Data(), this->service->Data());
			} else {
				this->logger->log_message(Log::Debug,
					L"<received confirmation(%hu, %hu, %hu, %hhu) for function 0x%02X comes from %s:%s>",
					transaction, protocol, length, unit, function_code,
					this->device->RawName->Data(), this->service->Data());
			}

			if (cf != nullptr) {
				if (function_code != raw_code) {
					cf->on_exception(transaction, function_code, maybe_address, this->mbin->ReadByte(), this->logger);
				} else {
					modbus_apply_positive_confirmation(cf, this->logger, this->mbin, transaction, function_code, maybe_address);
				}
			}
		});
	}).then([=](task<void> confirm) {
		try {
			confirm.get();

			unsigned int dirty = mbin->UnconsumedBufferLength;

			if (dirty > 0) {
				MODBUS_DISCARD_BYTES(mbin, dirty);
				this->logger->log_message(Log::Debug,
					L"<discarded last %u bytes of the confirmation comes from %s:%s>",
					dirty, this->device->RawName->Data(), this->service->Data());
			}

			this->wait_process_confirm_loop();
		} catch (modbus_discarded&) {
			unsigned int rest = mbin->UnconsumedBufferLength;
			
			if (rest > 0) {
				MODBUS_DISCARD_BYTES(mbin, rest);
			}
			
			this->wait_process_confirm_loop();
		} catch (task_canceled&) {
			this->connect();
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->connect();
		}
	});
}

bool IModbusClient::connected() {
	return (this->mbout != nullptr);
}

uint16 IModbusClient::do_simple_request(uint8 fcode, uint16 addr, uint16 val, IModbusConfirmation* cf) {
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, addr);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, val);

	return this->request(fcode, pdu_data, 4, cf);
}

uint16 IModbusClient::do_write_registers(uint8* pdu_data, uint8 offset
	, uint8 fcode, uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* cf) {
	uint8* data = pdu_data + offset;
	uint8 NStar = MODBUS_REGISTER_NStar(quantity);

	MODBUS_SET_INT16_TO_INT8(data, 0, address);
	MODBUS_SET_INT16_TO_INT8(data, 2, quantity);
	data[4] = NStar;
	modbus_read_registers(src, 0, NStar, data + 5);

	return this->request(fcode, pdu_data, offset + 5 + NStar, cf);
}

/*************************************************************************************************/
uint16 ModbusClient::read_coils(uint16 address, uint16 quantity, IModbusConfirmation* alt_confirmation) { // MAP: Page 11
	return IModbusClient::do_simple_request(MODBUS_READ_COILS, address, quantity, alt_confirmation);
}

uint16 ModbusClient::read_discrete_inputs(uint16 address, uint16 quantity, IModbusConfirmation* alt_confirmation) { // MAP: Page 12
	return IModbusClient::do_simple_request(MODBUS_READ_DISCRETE_INPUTS, address, quantity, alt_confirmation);
}

uint16 ModbusClient::write_coil(uint16 address, bool value, IModbusConfirmation* alt_confirmation) { // MAP: Page 17
	return IModbusClient::do_simple_request(MODBUS_WRITE_SINGLE_COIL, address, (value ? 0xFF00 : 0x0000), alt_confirmation);
}

uint16 ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* src, IModbusConfirmation* alt_confirmation) { // MAP: Page 29
	uint8* pdu_data = this->calloc_pdu();
	uint8 NStar = MODBUS_COIL_NStar(quantity);

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, quantity);
	pdu_data[4] = NStar;
	memcpy(pdu_data + 5, src, NStar);
	
	return IModbusClient::request(MODBUS_WRITE_MULTIPLE_COILS, pdu_data, 5 + NStar, alt_confirmation);
}

uint16 ModbusClient::read_holding_registers(uint16 address, uint16 quantity, IModbusConfirmation* alt_confirmation) {
	return IModbusClient::do_simple_request(MODBUS_READ_HOLDING_REGISTERS, address, quantity, alt_confirmation);
}

uint16 ModbusClient::read_input_registers(uint16 address, uint16 quantity, IModbusConfirmation* alt_confirmation) {
	return IModbusClient::do_simple_request(MODBUS_READ_INPUT_REGISTERS, address, quantity, alt_confirmation);
}

uint16 ModbusClient::read_queues(uint16 address, IModbusConfirmation* alt_confirmation) {
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);

	return IModbusClient::request(MODBUS_READ_FIFO_QUEUES, pdu_data, 2, alt_confirmation);
}


uint16 ModbusClient::write_register(uint16 address, uint16 value, IModbusConfirmation* alt_confirmation) { // MAP: Page 19
	return IModbusClient::do_simple_request(MODBUS_WRITE_SINGLE_REGISTER, address, value, alt_confirmation);
}

uint16 ModbusClient::write_registers(uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* alt_confirmation) { // MAP: Page 30
	uint8* pdu_data = this->calloc_pdu();

	return IModbusClient::do_write_registers(pdu_data, 0, MODBUS_WRITE_MULTIPLE_REGISTERS, address, quantity, src, alt_confirmation);
}

uint16 ModbusClient::mask_write_register(uint16 address, uint16 and, uint16 or, IModbusConfirmation* alt_confirmation) { // MAP: Page 36
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, address);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, and);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 4, or);

	return IModbusClient::request(MODBUS_MASK_WRITE_REGISTER, pdu_data, 6, alt_confirmation);
}

uint16 ModbusClient::write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src, IModbusConfirmation* alt_confirmation) {
	uint8* pdu_data = this->calloc_pdu();

	MODBUS_SET_INT16_TO_INT8(pdu_data, 0, raddr);
	MODBUS_SET_INT16_TO_INT8(pdu_data, 2, rquantity);

	return IModbusClient::do_write_registers(pdu_data, 4, MODBUS_WRITE_AND_READ_REGISTERS, waddr, wquantity, src, alt_confirmation);
}

uint16 ModbusClient::do_private_function(uint8 fcode, uint8* request, uint16 data_length, IModbusConfirmation* alt_confirmation) {
	// TODO: how to manage memory for raw requests.
	return IModbusClient::request(fcode, request, data_length, alt_confirmation);
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
