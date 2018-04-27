#pragma once

#include <ppltasks.h>

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	template<class Instance>
	private class IMsAppxlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	protected:
		virtual void on_appx(Windows::Foundation::Uri^ ms_appx, Instance^ instance) = 0;

	protected:
		void load_async(Windows::Foundation::Uri^ ms_appx, Concurrency::cancellation_token& token, Platform::String^ file_type = "graphlet source") {
			auto get_file = Concurrency::create_task(Windows::Storage::StorageFile::GetFileFromApplicationUriAsync(ms_appx), token);

			get_file.then([=](Concurrency::task<Windows::Storage::StorageFile^> file) {
				this->get_logger()->log_message(Log::Debug,
					L"Found the %s: %s",
					file_type->Data(),
					ms_appx->ToString()->Data());

				return Concurrency::create_task(file.get()->OpenAsync(Windows::Storage::FileAccessMode::Read,
					Windows::Storage::StorageOpenOptions::AllowOnlyReaders),
					token);
			}).then([=](Concurrency::task<Windows::Storage::Streams::IRandomAccessStream^> stream) {
				auto ds = Microsoft::Graphics::Canvas::CanvasDevice::GetSharedDevice();

				return Concurrency::create_task(Instance::LoadAsync(ds, stream.get()), token);
			}).then([=](Concurrency::task<Instance^> doc) {
				try {
					this->on_appx(ms_appx, doc.get());
					this->info->master->notify_graphlet_ready(this);
				} catch (Platform::Exception^ e) {
					this->get_logger()->log_message(WarGrey::SCADA::Log::Error,
						L"Failed to	load %s: %s",
						ms_appx->ToString()->Data(),
						e->Message->Data());
				} catch (Concurrency::task_canceled&) {
					this->get_logger()->log_message(WarGrey::SCADA::Log::Debug,
						L"Cancelled loading	%s",
						ms_appx->ToString()->Data());
				}
			});
		}
	};
}
