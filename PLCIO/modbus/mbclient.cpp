#include <ppltasks.h>

#include "modbus/mbclient.hpp"
#include "modbus/dataunit.hpp"
#include "modbus/exception.hpp"

#include "shared/netexn.hpp"

#include "syslog.hpp"

// MMIG: Page 20

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

#define POP_REQUEST(self) auto it = self.begin(); delete it->second; self.erase(it);

private struct WarGrey::SCADA::ModbusTransaction {
	uint8 function_code;
	uint8 pdu_data[MODBUS_MAX_PDU_LENGTH];
	uint16 size;
	uint16 address0;
};

inline static ModbusTransaction* make_transaction(uint8 fcode, uint16 address, uint16 size = 0) {
	ModbusTransaction* mt = new ModbusTransaction();

	mt->function_code = fcode;
	mt->size = size;
	mt->address0 = address;

	return mt;
}

static void modbus_apply_positive_confirmation(std::list<IModbusConfirmation*> cfs, Syslog* logger, DataReader^ mbin
	, uint16 transaction, uint8 function_code, uint16 maybe_address) {
	switch (function_code) {
	case MODBUS_READ_COILS: case MODBUS_READ_DISCRETE_INPUTS: {               // MAP: Page 11, 12
		static uint8 status[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte();
		READ_BYTES(mbin, status, count);

		if (function_code == MODBUS_READ_COILS) {
			for (auto cf : cfs) {
				cf->on_coils(transaction, maybe_address, status, count, logger);
			}
		} else {
			for (auto cf : cfs) {
				cf->on_discrete_inputs(transaction, maybe_address, status, count, logger);
			}
		}
	} break;
	case MODBUS_READ_HOLDING_REGISTERS: case MODBUS_READ_INPUT_REGISTERS:     // MAP: Page 15, 16
	case MODBUS_WRITE_AND_READ_REGISTERS: {                                   // MAP: Page 38
		static uint16 registers[MODBUS_MAX_PDU_LENGTH];
		uint8 count = mbin->ReadByte() / 2;
		READ_WORDS(mbin, registers, count);

		if (function_code == MODBUS_READ_INPUT_REGISTERS) {
			for (auto cf : cfs) {
				cf->on_input_registers(transaction, maybe_address, registers, count, logger);
			}
		} else {
			for (auto cf : cfs) {
				cf->on_holding_registers(transaction, maybe_address, registers, count, logger);
			}
		}
	} break;
	case MODBUS_WRITE_SINGLE_COIL: case MODBUS_WRITE_SINGLE_REGISTER:         // MAP: Page 17, 19
	case MODBUS_WRITE_MULTIPLE_COILS: case MODBUS_WRITE_MULTIPLE_REGISTERS: { // MAP: Page 29, 30
		uint16 address = mbin->ReadUInt16();
		uint16 value = mbin->ReadUInt16();

		for (auto cf : cfs) {
			cf->on_echo_response(transaction, function_code, address, value, logger);
		}
	} break;
	case MODBUS_MASK_WRITE_REGISTER: {                                        // MAP: Page 36
		uint16 address = mbin->ReadUInt16();
		uint16 and_mask = mbin->ReadUInt16();
		uint16 or_mask = mbin->ReadUInt16();

		for (auto cf : cfs) {
			cf->on_echo_response(transaction, function_code, address, and_mask, or_mask, logger);
		}
	} break;
	case MODBUS_READ_FIFO_QUEUES: {                                           // MAP: Page 40
		static uint16 queues[MODBUS_MAX_PDU_LENGTH];
		uint16 useless = mbin->ReadUInt16();
		uint16 count = mbin->ReadUInt16();

		for (auto cf : cfs) {
			cf->on_queue_registers(transaction, maybe_address, queues, count, logger);
		}
	} break;
	default: {
		static uint8 raw_data[MODBUS_MAX_PDU_LENGTH];
		static uint8 count = (uint8)mbin->UnconsumedBufferLength;

		READ_BYTES(mbin, raw_data, count);
		
		for (auto cf : cfs) {
			cf->on_private_response(transaction, function_code, raw_data, count, logger);
		}
	}
	}
}

/*************************************************************************************************/
IModbusClient::IModbusClient(Syslog* sl, Platform::String^ h, uint16 p, IModbusConfirmation* cf, IModbusTransactionIdGenerator* g) {
	this->logger = ((sl == nullptr) ? make_silent_logger("Silent Modbus Client") : sl);
	this->logger->reference();

	this->device = ref new HostName(h);
    this->service = p.ToString();

	this->append_confirmation_receiver(cf);
	this->generator = ((g == nullptr) ? new WarGrey::SCADA::ModbusSequenceGenerator() : g);
	this->generator->reference();

    this->connect();
};

IModbusClient::~IModbusClient() {
	delete this->socket; // stop the confirmation loop before release transactions.

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

	this->logger->destroy();
	this->generator->destroy();
}

Platform::String^ IModbusClient::device_hostname() {
	return this->device->RawName;
}

Syslog* IModbusClient::get_logger() {
	return this->logger;
}

void IModbusClient::append_confirmation_receiver(IModbusConfirmation* confirmation) {
	if (confirmation != nullptr) {
		this->confirmations.push_back(confirmation);
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
				auto current = this->blocking_requests.begin();

				this->apply_request(std::pair<uint16, ModbusTransaction*>(current->first, current->second));
				this->blocking_requests.erase(current);
			}
			this->blocking_section.unlock();
        } catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, socket_strerror(e));
			this->connect();
        }
    });
}

uint16 IModbusClient::request(ModbusTransaction* mt) {
	uint16 id = this->generator->yield();
	auto transaction = std::pair<uint16, ModbusTransaction*>(id, mt);

	if (this->connected()) {
		this->apply_request(transaction);
	} else {
		this->blocking_section.lock();
		this->blocking_requests.insert(transaction);
		this->blocking_section.unlock();
	}

	return id;
}

void IModbusClient::apply_request(std::pair<uint16, ModbusTransaction*>& transaction) {
	uint16 tid = transaction.first;
	uint8 fcode = transaction.second->function_code;

	this->pending_section.lock();
	this->pending_requests.insert(transaction);
	this->pending_section.unlock();

	modbus_write_adu(this->mbout, tid, 0x00, 0xFF, fcode, transaction.second->pdu_data, transaction.second->size);

	create_task(this->mbout->StoreAsync()).then([=](task<unsigned int> sending) {
		try {
			unsigned int sent = sending.get();

			this->logger->log_message(Log::Debug,
				L"<sent %u-byte-request for function 0x%02X as transaction %hu to %s:%s>",
				sent, fcode, tid, this->device->RawName->Data(), this->service->Data());
		} catch (task_canceled&) {
			this->pending_section.lock();
			this->pending_requests.erase(tid);
			this->pending_section.unlock();
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->pending_section.lock();
			this->pending_requests.erase(tid);
			this->pending_section.unlock();
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

		return create_task(this->mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
			uint16 address0 = 0;
			uint16 origin_fcode = 0;

			if (size < pdu_length) {
				modbus_protocol_fatal(this->logger,
					L"PDU data comes from server %s:%s has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, pdu_length);
			}

			auto id_transaction = this->pending_requests.find(transaction);
			if (id_transaction != this->pending_requests.end()) {	
				address0 = id_transaction->second->address0;
				origin_fcode = id_transaction->second->function_code;
				delete id_transaction->second;

				/** WARNING
				 *   `erase` also invalids its iterator, thus, this operation must occur at the end,
				 *   although, sometimes, the dirty iterator won't cause troubles.
				 */
				this->pending_section.lock();
				this->pending_requests.erase(id_transaction);
				this->pending_section.unlock();
			} else {
				modbus_discard_current_adu(this->logger,
					L"<discarded non-pending confirmation(%hu) comes from %s:%s>",
					transaction, this->device->RawName->Data(), this->service->Data());
			}

			if ((protocol != MODBUS_PROTOCOL) || (unit != MODBUS_TCP_SLAVE)) {
				// TODO: is it right to check it here?
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

			if (!this->confirmations.empty()) {
				if (function_code != raw_code) {
					for (auto c : this->confirmations) {
						c->on_exception(transaction, function_code, address0, this->mbin->ReadByte(), this->logger);
					}
				} else {
					modbus_apply_positive_confirmation(this->confirmations, this->logger, this->mbin, transaction, function_code, address0);
				}
			}
		});
	}).then([=](task<void> confirm) {
		try {
			confirm.get();

			unsigned int dirty = discard_dirty_bytes(this->mbin);

			if (dirty > 0) {
				this->logger->log_message(Log::Debug,
					L"<discarded last %u bytes of the confirmation comes from %s:%s>",
					dirty, this->device->RawName->Data(), this->service->Data());
			}

			this->wait_process_confirm_loop();
		} catch (task_discarded&) {
			discard_dirty_bytes(this->mbin);
			this->wait_process_confirm_loop();
		} catch (task_terminated&) {
			this->connect();
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

uint16 IModbusClient::do_simple_request(uint8 fcode, uint16 addr, uint16 val) {
	ModbusTransaction* mt = make_transaction(fcode, addr, 4);

	SET_INT16_TO_INT8(mt->pdu_data, 0, addr);
	SET_INT16_TO_INT8(mt->pdu_data, 2, val);

	return this->request(mt);
}

uint16 IModbusClient::do_write_registers(ModbusTransaction* mt, uint8 offset, uint16 address, uint16 quantity, uint16* src) {
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
uint16 ModbusClient::read_coils(uint16 address, uint16 quantity) { // MAP: Page 11
	return IModbusClient::do_simple_request(MODBUS_READ_COILS, address, quantity);
}

uint16 ModbusClient::read_discrete_inputs(uint16 address, uint16 quantity) { // MAP: Page 12
	return IModbusClient::do_simple_request(MODBUS_READ_DISCRETE_INPUTS, address, quantity);
}

uint16 ModbusClient::write_coil(uint16 address, bool value) { // MAP: Page 17
	return IModbusClient::do_simple_request(MODBUS_WRITE_SINGLE_COIL, address, (value ? 0xFF00 : 0x0000));
}

uint16 ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* src) { // MAP: Page 29
	uint8 NStar = MODBUS_COIL_NStar(quantity);
	ModbusTransaction* mt = make_transaction(MODBUS_WRITE_MULTIPLE_COILS, address, 5 + NStar);
	uint8* pdu_data = mt->pdu_data;

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);
	SET_INT16_TO_INT8(mt->pdu_data, 2, quantity);
	pdu_data[4] = NStar;
	memcpy(pdu_data + 5, src, NStar);
	
	return IModbusClient::request(mt);
}

uint16 ModbusClient::read_holding_registers(uint16 address, uint16 quantity) {
	return IModbusClient::do_simple_request(MODBUS_READ_HOLDING_REGISTERS, address, quantity);
}

uint16 ModbusClient::read_input_registers(uint16 address, uint16 quantity) {
	return IModbusClient::do_simple_request(MODBUS_READ_INPUT_REGISTERS, address, quantity);
}

uint16 ModbusClient::read_queues(uint16 address) {
	ModbusTransaction* mt = make_transaction(MODBUS_READ_FIFO_QUEUES, address, 2);

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);

	return IModbusClient::request(mt);
}


uint16 ModbusClient::write_register(uint16 address, uint16 value) { // MAP: Page 19
	return IModbusClient::do_simple_request(MODBUS_WRITE_SINGLE_REGISTER, address, value);
}

uint16 ModbusClient::write_registers(uint16 address, uint16 quantity, uint16* src) { // MAP: Page 30
	ModbusTransaction* mt = make_transaction(MODBUS_WRITE_MULTIPLE_REGISTERS, address);

	return IModbusClient::do_write_registers(mt, 0, address, quantity, src);
}

uint16 ModbusClient::mask_write_register(uint16 address, uint16 and, uint16 or) { // MAP: Page 36
	ModbusTransaction* mt = make_transaction(MODBUS_MASK_WRITE_REGISTER, address, 6);

	SET_INT16_TO_INT8(mt->pdu_data, 0, address);
	SET_INT16_TO_INT8(mt->pdu_data, 2, and);
	SET_INT16_TO_INT8(mt->pdu_data, 4, or);

	return IModbusClient::request(mt);
}

uint16 ModbusClient::write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src) {
	ModbusTransaction* mt = make_transaction(MODBUS_WRITE_AND_READ_REGISTERS, raddr);

	SET_INT16_TO_INT8(mt->pdu_data, 0, raddr);
	SET_INT16_TO_INT8(mt->pdu_data, 2, rquantity);

	return IModbusClient::do_write_registers(mt, 4, waddr, wquantity, src);
}

uint16 ModbusClient::do_private_function(uint8 fcode, uint8* request, uint16 data_length) {
	ModbusTransaction* mt = make_transaction(fcode, 0, data_length);

	// TODO: find a better way to deal with raw requests.
	memcpy(mt->pdu_data, request, data_length);
	return IModbusClient::request(mt);
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
