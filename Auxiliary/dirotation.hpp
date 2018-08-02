#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private enum class Rotation { Hourly, Daily, Monthly, Yearly };

	private class IRotativeDirectory {
	public:
		virtual ~IRotativeDirectory() noexcept;

		IRotativeDirectory(Platform::String^ dirname,
			WarGrey::SCADA::Rotation period = Rotation::Daily,
			unsigned int period_count = 1);

	protected:
		virtual void on_exception(Platform::Exception^ e);
		virtual void log_message(WarGrey::SCADA::Log leve, Platform::String^ message);

	protected:
		virtual void on_folder_ready(Platform::String^ path, bool newly_created) {}
		virtual void on_file_rotating(Platform::String^ current_file) {}
		virtual void on_file_rotated(Platform::String^ old_file, Platform::String^ current_file) {}

	private:
		void detect_time();

	private:
		Windows::Globalization::Calendar^ next;
		Windows::Storage::StorageFolder^ root;

	private:
		WarGrey::SCADA::Rotation period;
		unsigned int count;
	};
}
