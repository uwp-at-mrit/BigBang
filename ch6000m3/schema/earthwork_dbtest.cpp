#include "schema/earthwork_dbtest.hpp"
#include "schema/earthwork.hpp"

#include "dirotation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Storage;

static void dbtest(SQLite3* target) {
	auto sqlite3 = ((target == nullptr) ? new SQLite3() : target);
	EarthWork ew = make_earthwork();
	EarthWork eworks[2];
	EarthWork_pk id = earthwork_identity(ew);

	default_earthwork(eworks[0]);
	default_earthwork(eworks[1]);

	create_earthwork(sqlite3, false);
	sqlite3->table_info("earthwork");

	insert_earthwork(sqlite3, ew);
	insert_earthwork(sqlite3, ew); // UNIQUE constraint failed

	eworks[0].product = 4000.0;
	eworks[0].uuid = 42;
	update_earthwork(sqlite3, eworks[0]); // not such record in database

	insert_earthwork(sqlite3, eworks[0]);
	insert_earthwork(sqlite3, eworks[1]);

	sqlite3->get_logger()->log_message(Log::Notice,
		L"datum count: %lld, average: %lf, earliest: %X, lastest: %X",
		earthwork_count(sqlite3), earthwork_average(sqlite3),
		(int64)(earthwork_min(sqlite3, earthwork::uuid).value_or(0.0)),
		(int64)(earthwork_max(sqlite3, earthwork::uuid).value_or(0.0)));

	delete_earthwork(sqlite3, id);
	if (!seek_earthwork(sqlite3, id).has_value()) {
		sqlite3->get_logger()->log_message(Log::Info, "`seek_table` works for absent record");
	}

	auto aes = list_earthwork(sqlite3);
	for (auto lt = aes.begin(); lt != aes.end(); lt++) {
		EarthWork_pk pk = (*lt);
		std::optional<EarthWork> maybe_e = seek_earthwork(sqlite3, pk);

		if (maybe_e.has_value()) {
			EarthWork e = maybe_e.value();

			sqlite3->get_logger()->log_message(Log::Info,
				L"%X, %lf, %lf, %lf, %lf, %d", e.uuid,
				e.product, e.vessel, e.hopper_height,
				e.loading, e.replacement, e.timestamp);
		}
	}

	sqlite3->list_tables();
	drop_earthwork(sqlite3);
	sqlite3->table_info("earthwork");
	create_earthwork(sqlite3, true);
}

/*************************************************************************************************/
private class RotativeEarthWork : public IRotativeDirectory {
public:
	virtual ~RotativeEarthWork() {
		if (this->dbc != nullptr) {
			delete dbc;
		}

		this->logger->destroy();
	}

	RotativeEarthWork() : IRotativeDirectory("earthwork", RotationPeriod::Minutely) {
		this->logger = make_system_logger(Log::Debug, "RotativeEarthWork");
		this->logger->reference();
	}

protected:
	void on_file_rotated(StorageFile^ prev, StorageFile^ current) override {
		if (this->dbc != nullptr) {
			delete this->dbc;
		}

		this->dbc = new SQLite3(current->Path->Data(), this->logger);
		dbtest(this->dbc);
	}

private:
	Syslog* logger;
	SQLite3* dbc;
};

void WarGrey::SCADA::earthwork_dbtest() {
	// TODO: find out a solution to destruct the instance.
	new RotativeEarthWork();
}
