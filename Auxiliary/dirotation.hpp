#pragma once

#include <ppltasks.h>

#include "time.hpp"

namespace WarGrey::SCADA {
	private enum class RotationPeriod { Daily, Hourly, Minutely, Secondly };

	private class IRotativeDirectory abstract {
	public:
		virtual ~IRotativeDirectory() noexcept;

		IRotativeDirectory(Platform::String^ dirname,
			WarGrey::SCADA::RotationPeriod period = RotationPeriod::Daily, unsigned int period_count = 1,
			Platform::String^ file_prefix = nullptr, Platform::String^ file_suffix = ".db");

	public:
		virtual Platform::String^ resolve_filename(long long time_100ns = current_100nanoseconds());

	public:
		void do_rotating_with_this_bad_named_function_which_not_designed_for_client_applications();

	protected:
		virtual void on_folder_ready(Windows::Storage::StorageFolder^ path, bool newly_folder) {}
		virtual void on_file_reused(Windows::Storage::StorageFile^ current_file);
		virtual void on_file_rotated(Windows::Storage::StorageFile^ prev_file, Windows::Storage::StorageFile^ current_file) = 0;

	protected:
		virtual void on_exception(Platform::Exception^ e);

	private:
		void mission_start();
		Windows::Foundation::TimeSpan resolve_interval();
		
	private:
		Concurrency::cancellation_token_source destructing_watcher;
		Windows::Storage::StorageFolder^ root;
		Windows::Storage::StorageFile^ current_file;
		Platform::String^ file_prefix;
		Platform::String^ file_suffix;

	private:
		WarGrey::SCADA::RotationPeriod period;
		unsigned int period_count;

	private:
		Windows::UI::Xaml::DispatcherTimer^ timer;
		Platform::Object^ listener;
		long long span;
	};
}
