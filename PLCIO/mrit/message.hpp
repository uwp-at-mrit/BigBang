#pragma once

#include "shared/databytes.hpp"

namespace WarGrey::SCADA {
	private class MrMessageConfiguration final {
	public:
		MrMessageConfiguration(size_t dball = 98, size_t alignment_size = 40U, size_t old_protocol_data_size = 0);

	public:
		bool is_old_protocol();

	public:
		void set_fcode(char read_signal = 'A', char write_analog_quantity = 'B', char write_digital_quantity = 'C');
		char read_signal_fcode();
		char write_analog_quantity_fcode();
		char write_digital_quantity_fcode();

		size_t read_all_dbcode();

	public:
		void set_header(size_t value, size_t size);
		void set_tail(size_t value, size_t size);
		void set_alignment_size(size_t size);

	public:
		void set_command_size(size_t size);
		void set_datablock_slot_size(size_t size);
		void set_start_address_size(size_t size);
		void set_end_address_size(size_t size);
		void set_datasize_size(size_t size);
		void set_checksum_size(size_t size);

	public:
		size_t predata_size();
		size_t postdata_size();

	public:
		bool header_match(size_t header, size_t* expected = nullptr);
		bool tail_match(size_t tail, size_t* expected = nullptr);

	public:
		size_t read_header(Windows::Storage::Streams::IDataReader^ mrin,
			size_t* head, size_t* fcode, size_t* db_id, size_t* addr0, size_t* addrn, size_t* size);
		
		void read_body_tail(Windows::Storage::Streams::IDataReader^ mrin, size_t size, uint8* data,
			size_t* checksum, size_t* eom);

		void write_header(Windows::Storage::Streams::IDataWriter^ mrout, size_t fcode, size_t db_id, size_t addr0, size_t addrn);
		void write_body_tail(Windows::Storage::Streams::IDataWriter^ mrout, uint8* data, size_t size);
		void write_aligned_tail(Windows::Storage::Streams::IDataWriter^ mrout, uint8* data, size_t size);

	private:
		size_t header_value;
		size_t tail_value;
		size_t alignment_size;

	private:
		size_t header_size;
		size_t fcode_size;
		size_t dbid_size;
		size_t addr0_size;
		size_t addrn_size;
		size_t datasize_size;
		size_t checksum_size;
		size_t tail_size;

	private:
		size_t db_read_all;
		char read_signal;
		char write_analog_quantity;
		char write_digital_quantity;

	private:
		size_t old_protocol_data_size;

	private:
		unsigned int header_checksum;

	private:
		uint8* body;
	};
}
