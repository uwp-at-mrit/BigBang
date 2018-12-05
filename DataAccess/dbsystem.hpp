#pragma once

#include <map>
#include <list>
#include <optional>

#include "syslog.hpp"
#include "vsql.hpp"

namespace WarGrey::SCADA {
#define DeclareQueryValue(type, value) \
        std::optional<type> query_maybe_##value(const std::string& sql); \
        std::optional<type> query_maybe_##value(const char* sql, ...); \
        type query_##value(const std::string& sql); \
        type query_##value(const char* sql, ...)

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

	public:
		virtual std::string column_text(unsigned int cid_starts_with_0) = 0;
		virtual int32 column_int32(unsigned int cid_starts_with_0) = 0;
		virtual int64 column_int64(unsigned int cid_starts_with_0) = 0;
		virtual double column_double(unsigned int cid_starts_with_0) = 0;

	public:
		virtual int column_data_count() = 0;
		virtual bool column_is_null(unsigned int cid_starts_with_0) = 0;
		virtual std::string column_database_name(unsigned int cid_starts_with_0) = 0;
		virtual std::string column_table_name(unsigned int cid_starts_with_0) = 0;
		virtual std::string column_name(unsigned int cid_starts_with_0) = 0;
		virtual std::string column_decltype(unsigned int cid_starts_with_0) = 0;

	public:
		virtual std::string description(bool expand = true) = 0;
		virtual bool step(int* data_count = nullptr, const char* error_src = "step") = 0;
		virtual void reset(bool reset_bindings = true) = 0;
		virtual void clear_bindings() = 0;
		
	public:
		void bind_parameter(unsigned int pid_starts_with_0, float v);
		void bind_parameter(unsigned int pid_starts_with_0, const std::string& blob);
		void bind_parameter(unsigned int pid_starts_with_0, Platform::String^ text);

	public:
		std::optional<std::string> column_maybe_text(unsigned int cid_starts_with_0);
		std::optional<int32> column_maybe_int32(unsigned int cid_starts_with_0);
		std::optional<int64> column_maybe_int64(unsigned int cid_starts_with_0);
		std::optional<double> column_maybe_double(unsigned int cid_starts_with_0);

	public:
		template <typename T>
		void bind_parameter(unsigned int pid_starts_with_0, std::optional<T> v) {
			if (v.has_value()) {
				this->bind_parameter(pid_starts_with_0, v.value());
			} else {
				this->bind_parameter(pid_starts_with_0);
			}
		}
	};
	
	private class IDBSystem : public WarGrey::SCADA::IDBObject {
	public:
		virtual ~IDBSystem() noexcept;

		IDBSystem(WarGrey::SCADA::DBMS dbms, WarGrey::SCADA::Syslog* logger);

	public:
		WarGrey::SCADA::Syslog* get_logger();
		WarGrey::SCADA::IVirtualSQL* make_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count);

		template<size_t N>
		WarGrey::SCADA::IVirtualSQL* make_sql_factory(WarGrey::SCADA::TableColumnInfo (&columns)[N]) {
			return this->make_sql_factory(columns, N);
		}

	public:
		virtual std::list<std::string> list_tables() = 0;
		virtual bool table_exists(const std::string& tablename);
		virtual std::string last_error_message() = 0;
		virtual int last_errno(int* extended_errno = nullptr) = 0;
		virtual WarGrey::SCADA::IPreparedStatement* prepare(const std::string& sql) = 0;
		
	public:
		WarGrey::SCADA::IPreparedStatement* prepare(const char* sql, ...);
		
		void exec(WarGrey::SCADA::IPreparedStatement* stmt);
		void exec(const std::string& sql);
		void exec(const char* sql, ...);

		DeclareQueryValue(std::string, text);
		DeclareQueryValue(int32, int32);
		DeclareQueryValue(int64, int64);
		DeclareQueryValue(double, double);

	public:
		void report_error();
		void report_error(const std::string& msg_prefix);
		void report_error(const char* format, ...);

		void report_warning();
		void report_warning(const std::string& msg_prefix);
		void report_warning(const char* format, ...);

	protected:
		virtual WarGrey::SCADA::IVirtualSQL* new_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) = 0;

	private:
		void log(WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);
		void log(const std::string& msg_prefix, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);

	private:
		WarGrey::SCADA::Syslog* logger;
		std::map<WarGrey::SCADA::TableColumnInfo*, WarGrey::SCADA::IVirtualSQL*> factories;
	};
}
