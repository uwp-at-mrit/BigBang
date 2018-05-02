#pragma once

#include <ppltasks.h>
#include <mutex>
#include <queue>
#include <map>

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	template<class FileType>
	private class IMsAppxlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		virtual ~IMsAppxlet() noexcept {
			this->shared_task.cancel();
		}

	protected:
		virtual void on_appx(Windows::Foundation::Uri^ ms_appx, FileType^ instance) = 0;

	protected:
		virtual Windows::Foundation::IAsyncOperation<FileType^>^ read(Windows::Storage::Streams::IRandomAccessStream^ stream) {
			return FileType::LoadAsync(Microsoft::Graphics::Canvas::CanvasDevice::GetSharedDevice(), stream);
		}

	protected:
		void load(Windows::Foundation::Uri^ ms_appx, Platform::String^ file_type = "graphlet source") {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppxlet<FileType>::critical_section.lock();
			auto reference = IMsAppxlet<FileType>::refcounts.find(uuid);

			if (reference == IMsAppxlet<FileType>::refcounts.end()) {
				IMsAppxlet<FileType>::refcounts[uuid] = 0;
				IMsAppxlet<FileType>::queues[uuid].push(this);
				this->load_async(uuid, ms_appx, file_type);
			} else if (reference->second > 0) {
				reference->second += 1;
				this->on_appx(ms_appx, IMsAppxlet<FileType>::filesystem[uuid]);

				this->get_logger()->log_message(Log::Debug, L"reused the %s: %s with reference count %d",
					file_type->Data(), ms_appx->ToString()->Data(),
					IMsAppxlet<FileType>::refcounts[uuid]);
			} else {
				IMsAppxlet<FileType>::queues[uuid].push(this);
				this->get_logger()->log_message(Log::Debug, L"waiting for the %s: %s",
					file_type->Data(), ms_appx->ToString()->Data());
			}
			IMsAppxlet<FileType>::critical_section.unlock();
		}

		void unload(Windows::Foundation::Uri^ ms_appx) {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppxlet<FileType>::critical_section.lock();
			auto reference = IMsAppxlet<FileType>::refcounts.find(uuid);

			if (reference != IMsAppxlet<FileType>::refcounts.end()) {
				if (reference->second <= 1) {
					IMsAppxlet<FileType>::refcounts.erase(uuid);
					IMsAppxlet<FileType>::filesystem.erase(uuid);
				} else {
					reference->second -= 1;
				}
			}
			IMsAppxlet<FileType>::critical_section.unlock();
		}

	private:
		void load_async(int uuid, Windows::Foundation::Uri^ ms_appx, Platform::String^ file_type) {
			auto token = this->shared_task.get_token();
			auto get_file = Concurrency::create_task(Windows::Storage::StorageFile::GetFileFromApplicationUriAsync(ms_appx), token);

			get_file.then([=](Concurrency::task<Windows::Storage::StorageFile^> file) {
				this->get_logger()->log_message(Log::Debug, L"found the %s: %s",
					file_type->Data(), ms_appx->ToString()->Data());

				return Concurrency::create_task(file.get()->OpenAsync(Windows::Storage::FileAccessMode::Read,
					Windows::Storage::StorageOpenOptions::AllowOnlyReaders),
					token);
			}).then([=](Concurrency::task<Windows::Storage::Streams::IRandomAccessStream^> stream) {
				return Concurrency::create_task(this->read(stream.get()), token);
			}).then([=](Concurrency::task<FileType^> doc) {
				IMsAppxlet<FileType>::critical_section.lock();
				try {
					FileType^ fsobject = doc.get();
					std::queue<IMsAppxlet<FileType>*> q = IMsAppxlet<FileType>::queues[uuid];

					IMsAppxlet<FileType>::refcounts[uuid] = q.size();
					IMsAppxlet<FileType>::filesystem[uuid] = fsobject;

					while (!q.empty()) {
						auto self = q.front();

						self->on_appx(ms_appx, fsobject);
						self->info->master->notify_graphlet_ready(self);
						q.pop();
					}

					this->get_logger()->log_message(Log::Debug, L"loaded the %s: %s with reference count %d",
						file_type->Data(), ms_appx->ToString()->Data(),
						IMsAppxlet<FileType>::refcounts[uuid]);
					IMsAppxlet<FileType>::queues.erase(uuid);
				} catch (Platform::Exception^ e) {
					IMsAppxlet<FileType>::clear(uuid);
					this->get_logger()->log_message(WarGrey::SCADA::Log::Error, L"failed to	load %s: %s",
						ms_appx->ToString()->Data(), e->Message->Data());
				} catch (Concurrency::task_canceled&) {
					IMsAppxlet<FileType>::clear(uuid);
					this->get_logger()->log_message(WarGrey::SCADA::Log::Debug,
						L"cancelled loading %s", ms_appx->ToString()->Data());
				} catch (std::exception& e) {
					IMsAppxlet<FileType>::clear(uuid);
					this->get_logger()->log_message(WarGrey::SCADA::Log::Debug,
						L"unexcepted exception: %s", e.what());
				}
				IMsAppxlet<FileType>::critical_section.unlock();
			});
		}

	private:
		static void clear(int uuid) {
			std::queue<IMsAppxlet<FileType>*> q = IMsAppxlet<FileType>::queues[uuid];

			while (!q.empty()) {
				q.pop();
			}

			IMsAppxlet<FileType>::refcounts.erase(uuid);
			IMsAppxlet<FileType>::queues.erase(uuid);
		}

	private:
		Concurrency::cancellation_token_source shared_task;

	private:
		static std::map<int, size_t> refcounts;
		static std::map<int, FileType^> filesystem;
		static std::map<int, std::queue<IMsAppxlet<FileType>*>> queues;
		static std::mutex critical_section;
	};

	template<class FileType> std::map<int, size_t> IMsAppxlet<FileType>::refcounts;
	template<class FileType> std::map<int, FileType^> IMsAppxlet<FileType>::filesystem;
	template<class FileType> std::map<int, std::queue<IMsAppxlet<FileType>*>> IMsAppxlet<FileType>::queues;
	template<class FileType> std::mutex IMsAppxlet<FileType>::critical_section;
}
