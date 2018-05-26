#pragma once

#include <map>
#include <optional>

#include "syslog.hpp"
#include "vsql.hpp"

namespace WarGrey::SCADA {
	private enum class DBMS { SQLite3 };

	private class IDBObject {
	public:
		virtual ~IDBObject() noexcept {};

		IDBObject(WarGrey::SCADA::DBMS dbms) : dbms(dbms) {}

	public:
		WarGrey::SCADA::DBMS dbsystem() {
			return this->dbms;
		}

	private:
		WarGrey::SCADA::DBMS dbms;
	};

	private class IPreparedStatement : public WarGrey::SCADA::IDBObject {
	public:
		IPreparedStatement(WarGrey::SCADA::DBMS dbms) : IDBObject(dbms) {}

	public:
		virtual unsigned int parameter_count() = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0) = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0, int32 v) = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0, int64 v) = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0, double v) = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0, const char* blob) = 0;
		virtual void bind_parameter(unsigned int pid_starts_with_0, const wchar_t* text) = 0;

	public:
		virtual bool step(int* data_count = nullptr, const wchar_t* error_src = L"step") = 0;
		virtual int column_data_count() = 0;
		virtual Platform::String^ column_database_name(unsigned int cid_starts_with_0) = 0;
		virtual Platform::String^ column_table_name(unsigned int cid_starts_with_0) = 0;
		virtual Platform::String^ column_name(unsigned int cid_starts_with_0) = 0;
		virtual Platform::String^ column_decltype(unsigned int cid_starts_with_0) = 0;
		virtual std::string column_blob(unsigned int cid_starts_with_0) = 0;
		virtual Platform::String^ column_text(unsigned int cid_starts_with_0) = 0;
		virtual int32 column_int32(unsigned int cid_starts_with_0) = 0;
		virtual int64 column_int64(unsigned int cid_starts_with_0) = 0;
		virtual double column_double(unsigned int cid_starts_with_0) = 0;

	public:
		virtual void reset(bool reset_bindings = true) = 0;
		virtual void clear_bindings() = 0;
		virtual Platform::String^ description() = 0;

	public:
		void bind_parameter(unsigned int pid_starts_with_0, float v);
		void bind_parameter(unsigned int pid_starts_with_0, std::string blob);
		void bind_parameter(unsigned int pid_starts_with_0, Platform::String^ text);

	public:
		template <typename T>
		void bind_parameter(unsigned int pid_starts_with_0, std::optional<T> v) {
			if (bool(v)) {
				this->bind_parameter(pid_starts_with_0, v.value());
			} else {
				this->bind_parameter(pid_starts_with_0);
			}
		}

	public:
		float column_float(unsigned int cid_starts_with_0);
	};

	private class IDBSystem : public WarGrey::SCADA::IDBObject {
	public:
		virtual ~IDBSystem() noexcept;

		IDBSystem(WarGrey::SCADA::Syslog* logger, WarGrey::SCADA::DBMS dbms);

	public:
		WarGrey::SCADA::Syslog* get_logger();
		WarGrey::SCADA::IVirtualSQL* make_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count);

	public:
		virtual const wchar_t* get_last_error_message() = 0;
		virtual WarGrey::SCADA::IPreparedStatement* prepare(Platform::String^ sql) = 0;
		
	public:
		WarGrey::SCADA::IPreparedStatement* prepare(const wchar_t* sql, ...);
		void exec(WarGrey::SCADA::IPreparedStatement* stmt);
		void exec(Platform::String^ stmt);
		void exec(const wchar_t* sql, ...);

	public:
		void report_error(Platform::String^ msg_prefix = nullptr);
		void report_error(const wchar_t* format, ...);

		void report_warning(Platform::String^ msg_prefix = nullptr);
		void report_warning(const wchar_t* format, ...);

	protected:
		virtual WarGrey::SCADA::IVirtualSQL* new_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) = 0;

	private:
		void log(Platform::String^ msg_prefix = nullptr, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);

	private:
		WarGrey::SCADA::Syslog* logger;
		std::map<WarGrey::SCADA::TableColumnInfo*, WarGrey::SCADA::IVirtualSQL*> factories;
	};
}
