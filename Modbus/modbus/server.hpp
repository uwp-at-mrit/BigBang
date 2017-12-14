#pragma once

#include <cinttypes>

namespace WarGrey::SCADA {
    private class IModbusServer abstract {
    public:
        virtual ~IModbusServer() noexcept;
		IModbusServer(uint16 port, const char* vendor_code, const char* product_code, const char* revision,
			const char* vendor_url, const char* product_name = nullptr, const char* model_name = nullptr,
			const char* application_name = nullptr);

    public:
        void listen();
		int request(uint8 function_code, Windows::Storage::Streams::DataReader^ mbin, uint8 *response);
		int process_device_identification(uint8* object_list, uint8 object, uint8 capacity, bool cut);
        void enable_debug(bool on_or_off);
        bool debug_enabled();

    public: // data access
		/* Bit access */
        virtual int read_coils(uint16 address, uint16 quantity, uint8* coil_status) = 0;
		virtual int read_discrete_inputs(uint16 address, uint16 quantity, uint8* input_status) = 0;
        virtual int write_coil(uint16 address, bool value) = 0;
        virtual int write_coils(uint16 address, uint16 quantity, uint8* src) = 0;

		/* 16 bits access */
		virtual int read_holding_registers(uint16 address, uint16 quantity, uint8* register_values) = 0;
		virtual int read_input_registers(uint16 address, uint16 quantity, uint8* input_registers) = 0;
		virtual int write_register(uint16 address, uint16 value) = 0;
		virtual int write_registers(uint16 address, uint16 quantity, uint8* src) = 0;
		virtual int mask_write_register(uint16 address, uint16 and, uint16 or) = 0;
		virtual int write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint8* rwpool) = 0;
		virtual int read_queues(uint16 address, uint8* value_registers) = 0;

		/* file record access */

    public: // Diagnostics
		virtual const char* access_private_device_identification(uint8 object) = 0;

    public: // Other
        virtual int do_private_function(uint8 function_code, uint8* request, uint16 request_data_length, uint8* response);

	protected:
		const char* standard_identifications[7];
        bool debug = false;

    private:
        Windows::Networking::Sockets::StreamSocketListener^ listener;
		Platform::String^ service;
    };
}
