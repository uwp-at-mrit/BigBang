#include <ppltasks.h>

#include "mrit/master.hpp"
#include "modbus/dataunit.hpp"
#include "modbus/exception.hpp"
#include "modbus/codes.hpp"

#include "shared/netexn.hpp"

#include "syslog.hpp"
#include "box.hpp"

// MMIG: Page 20

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

private struct WarGrey::SCADA::MRTransaction {
	uint8 function_code;
	uint8 pdu_data[MODBUS_MAX_PDU_LENGTH];
	uint16 size;
	uint16 address0;
};

inline static MRTransaction* make_transaction(uint8 fcode, uint16 address, uint16 size = 0) {
	MRTransaction* mt = new MRTransaction();

	mt->function_code = fcode;
	mt->size = size;
	mt->address0 = address;

	return mt;
}

static void modbus_apply_positive_confirmation(IMRConfirmation* cf, Syslog* logger, DataReader^ mbin
	, uint16 transaction, uint8 function_code, uint16 maybe_address) {
	switch (function_code) {
	case MODBUS_READ_COILS: case MODBUS_READ_DISCRETE_INPUTS: {               // MAP: Page 11, 12
		static uint8 status[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte();
		READ_BYTES(mbin, status, count);

		if (function_code == MODBUS_READ_COILS) {
			cf->on_coils(transaction, maybe_address, status, count, logger);
		} else {
			cf->on_discrete_inputs(transaction, maybe_address, status, count, logger);
		}
	} break;
	case MODBUS_READ_HOLDING_REGISTERS: case MODBUS_READ_INPUT_REGISTERS:     // MAP: Page 15, 16
	case MODBUS_WRITE_AND_READ_REGISTERS: {                                   // MAP: Page 38
		static uint16 registers[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte() / 2;
		READ_WORDS(mbin, registers, count);

		if (function_code == MODBUS_READ_INPUT_REGISTERS) {
			cf->on_input_registers(transaction, maybe_address, registers, count, logger);
		} else {
			cf->on_holding_registers(transaction, maybe_address, registers, count, logger);
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

		cf->on_queue_registers(transaction, maybe_address, queues, count, logger);
	} break;
	default: {
		static uint8 raw_data[MODBUS_MAX_PDU_LENGTH];
		static uint8 count = (uint8)mbin->UnconsumedBufferLength;

		READ_BYTES(mbin, raw_data, count);
		cf->on_private_response(transaction, function_code, raw_data, count, logger);
	}
	}
}

/*************************************************************************************************/
IMRClient::IMRClient(Syslog* sl, Platform::String^ h, uint16 p, IMRConfirmation* cf) {
	this->logger = ((sl == nullptr) ? make_silent_logger("Silent Modbus Client") : sl);
	this->logger->reference();

	this->device = ref new HostName(h);
    this->service = p.ToString();

	this->confirmation = cf;

    this->connect();
};

IMRClient::~IMRClient() {
	delete this->socket; // stop the confirmation loop before release transactions.

	this->blocking_section.lock();
	while (!this->blocking_requests.empty()) {
		this->blocking_requests.pop();
	}
	this->blocking_section.unlock();

	this->logger->destroy();
}

Platform::String^ IMRClient::device_hostname() {
	return this->device->RawName;
}

Syslog* IMRClient::get_logger() {
	return this->logger;
}

void IMRClient::send_scheduled_request(long long count, long long interval, long long uptime) {
	if (this->confirmation != nullptr) {
		this->confirmation->on_scheduled_request(this, count, interval, uptime);
	}
}

void IMRClient::connect() {
	if (this->mbout != nullptr) {
		delete this->socket;
		delete this->mbin;
		delete this->mbout;

		this->mbout = nullptr;
	}

	this->socket = ref new StreamSocket();
	this->socket->Control->KeepAlive = false;

	this->logger->log_message(Log::Debug, L">> connecting to %s:%s", this->device->RawName->Data(), this->service->Data());

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
				this->apply_request(this->blocking_requests.front());
				this->blocking_requests.pop();
			}
			this->blocking_section.unlock();
        } catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, socket_strerror(e));
			this->connect();
        }
    });
}

uint16 IMRClient::request(MRTransaction* mt) {
	if (this->connected()) {
		this->apply_request(mt);
	} else {
		this->blocking_section.lock();
		this->blocking_requests.push(mt);
		this->blocking_section.unlock();
	}

	return 0U;
}

void IMRClient::apply_request(MRTransaction* transaction) {
	uint16 tid = 0U;
	uint8 fcode = transaction->function_code;

	modbus_write_adu(this->mbout, tid, 0x00, 0xFF, fcode, transaction->pdu_data, transaction->size);

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

void IMRClient::wait_process_confirm_loop() {
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
			uint16 address0 = 0;
			uint16 origin_fcode = 0;

			if (size < pdu_length) {
				modbus_protocol_fatal(this->logger,
					L"PDU data comes from server %s:%s has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, pdu_length);
			}

			if ((protocol != MODBUS_PROTOCOL) || (unit != MODBUS_TCP_SLAVE)) {
				modbus_discard_current_adu(this->logger,
					L"<discarded non-modbus-tcp confirmation(%hu, %hu, %hu, %hhu) comes from %s:%s>",
					transaction, protocol, length, unit, this->device->RawName->Data(), this->service->Data());
			}

			uint8 raw_code = mbin->ReadByte();
			uint8 function_code = ((raw_code > 0x80) ? (raw_code - 0x80) : raw_code);

			if (function_code != origin_fcode) {
				modbus_discard_current_adu(this->logger,
					L"<discarded negative confirmation due to non-expected function(0x%02X) comes from %s:%s>",
					function_code, this->device->RawName->Data(), this->service->Data());
			} else {
				this->logger->log_message(Log::Debug,
					L"<received confirmation(%hu, %hu, %hu, %hhu) for function 0x%02X comes from %s:%s>",
					transaction, protocol, length, unit, function_code,
					this->device->RawName->Data(), this->service->Data());
			}

			if (this->confirmation != nullptr) {
				if (function_code != raw_code) {
					this->confirmation->on_exception(transaction, function_code, address0, this->mbin->ReadByte(), this->logger);
				} else {
					modbus_apply_positive_confirmation(this->confirmation, this->logger, this->mbin, transaction, function_code, address0);
				}
			}
		});
	}).then([=](task<void> confirm) {
		try {
			confirm.get();

			unsigned int dirty = mbin->UnconsumedBufferLength;

			if (dirty > 0) {
				DISCARD_BYTES(mbin, dirty);
				this->logger->log_message(Log::Debug,
					L"<discarded last %u bytes of the confirmation comes from %s:%s>",
					dirty, this->device->RawName->Data(), this->service->Data());
			}

			this->wait_process_confirm_loop();
		} catch (task_discarded&) {
			unsigned int rest = mbin->UnconsumedBufferLength;
			
			if (rest > 0) {
				DISCARD_BYTES(mbin, rest);
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

bool IMRClient::connected() {
	return (this->mbout != nullptr);
}

uint16 IMRClient::do_simple_request(uint8 fcode, uint16 addr, uint16 val) {
	MRTransaction* mt = make_transaction(fcode, addr, 4);

	SET_INT16_TO_INT8(mt->pdu_data, 0, addr);
	SET_INT16_TO_INT8(mt->pdu_data, 2, val);

	return this->request(mt);
}

uint16 IMRClient::do_write_registers(MRTransaction* mt, uint8 offset, uint16 address, uint16 quantity, uint16* src) {
	uint8* data = mt->pdu_data + offset;
	uint8 NStar = MODBUS_REGISTER_NStar(quantity);

	SET_INT16_TO_INT8(data, 0, address);
	SET_INT16_TO_INT8(data, 2, quantity);
	data[4] = NStar;
	read_words(src, 0, NStar, data + 5);
	
	mt->size = offset + 5 + NStar;

	return this->request(mt);
}

/*************************************************************************************************/
uint16 MRClient::read_coils(uint16 address, uint16 quantity) { // MAP: Page 11
	return IMRClient::do_simple_request(MODBUS_READ_COILS, address, quantity);
}

uint16 MRClient::read_discrete_inputs(uint16 address, uint16 quantity) { // MAP: Page 12
	return IMRClient::do_simple_request(MODBUS_READ_DISCRETE_INPUTS, address, quantity);
}

uint16 MRClient::write_coil(uint16 address, bool value) { // MAP: Page 17
	return IMRClient::do_simple_request(MODBUS_WRITE_SINGLE_COIL, address, (value ? 0xFF00 : 0x0000));
}

uint16 MRClient::write_coils(uint16 address, uint16 quantity, uint8* src) { // MAP: Page 29
	uint8 NStar = MODBUS_COIL_NStar(quantity);
	MRTransaction* mt = make_transaction(MODBUS_WRITE_MULTIPLE_COILS, address, 5 + NStar);
	uint8* pdu_data = mt->pdu_data;

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);
	SET_INT16_TO_INT8(mt->pdu_data, 2, quantity);
	pdu_data[4] = NStar;
	memcpy(pdu_data + 5, src, NStar);
	
	return IMRClient::request(mt);
}

uint16 MRClient::read_holding_registers(uint16 address, uint16 quantity) {
	return IMRClient::do_simple_request(MODBUS_READ_HOLDING_REGISTERS, address, quantity);
}

uint16 MRClient::read_input_registers(uint16 address, uint16 quantity) {
	return IMRClient::do_simple_request(MODBUS_READ_INPUT_REGISTERS, address, quantity);
}

uint16 MRClient::read_queues(uint16 address) {
	MRTransaction* mt = make_transaction(MODBUS_READ_FIFO_QUEUES, address, 2);

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);

	return IMRClient::request(mt);
}


uint16 MRClient::write_register(uint16 address, uint16 value) { // MAP: Page 19
	return IMRClient::do_simple_request(MODBUS_WRITE_SINGLE_REGISTER, address, value);
}

uint16 MRClient::write_registers(uint16 address, uint16 quantity, uint16* src) { // MAP: Page 30
	MRTransaction* mt = make_transaction(MODBUS_WRITE_MULTIPLE_REGISTERS, address);

	return IMRClient::do_write_registers(mt, 0, address, quantity, src);
}

uint16 MRClient::mask_write_register(uint16 address, uint16 and, uint16 or) { // MAP: Page 36
	MRTransaction* mt = make_transaction(MODBUS_MASK_WRITE_REGISTER, address, 6);

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);
	SET_INT16_TO_INT8(mt->pdu_data, 2, and);
	SET_INT16_TO_INT8(mt->pdu_data, 4, or);

	return IMRClient::request(mt);
}

uint16 MRClient::write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src) {
	MRTransaction* mt = make_transaction(MODBUS_WRITE_AND_READ_REGISTERS, raddr);

	SET_INT16_TO_INT8(mt->pdu_data, 0, raddr);
	SET_INT16_TO_INT8(mt->pdu_data, 2, rquantity);

	return IMRClient::do_write_registers(mt, 4, waddr, wquantity, src);
}

uint16 MRClient::do_private_function(uint8 fcode, uint8* request, uint16 data_length) {
	MRTransaction* mt = make_transaction(fcode, 0, data_length);

	// TODO: find a better way to deal with raw requests.
	memcpy(mt->pdu_data, request, data_length);
	return IMRClient::request(mt);
}
