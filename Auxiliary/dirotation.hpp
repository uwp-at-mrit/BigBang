#pragma once

#include <ppltasks.h>

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private enum class RotationPeriod { Daily, Hourly, Minutely, Secondly };

	private class IRotativeDirectory abstract {
	public:
		virtual ~IRotativeDirectory() noexcept;

		IRotativeDirectory(WarGrey::SCADA::Syslog* logger, Platform::String^ dirname,
			Platform::String^ file_prefix, Platform::String^ file_suffix = ".db",
			WarGrey::SCADA::RotationPeriod period = RotationPeriod::Minutely,
			unsigned int period_count = 1);

	public:
		void rotate_now();

	protected:
		virtual void on_exception(Platform::Exception^ e);
		virtual void log_message(WarGrey::SCADA::Log leve, Platform::String^ message);

	protected:
		virtual void on_folder_ready(Platform::String^ path, bool newly_folder) {}
		virtual void on_file_rotated(Platform::String^ old_file, Platform::String^ current_file) = 0;

	private:
		void mission_start();
		Platform::String^ resolve_current_file();
		Windows::Foundation::TimeSpan resolve_interval();
		
	private:
		Concurrency::cancellation_token_source destructing_watcher;
		Windows::Storage::StorageFolder^ root;
		Platform::String^ current_file;
		Platform::String^ file_prefix;
		Platform::String^ file_suffix;

	private:
		WarGrey::SCADA::Syslog* logger;
		WarGrey::SCADA::RotationPeriod period;
		unsigned int period_count;

	private:
		Windows::UI::Xaml::DispatcherTimer^ timer;
		Platform::Object^ listener;
		long long span;
	};
}
