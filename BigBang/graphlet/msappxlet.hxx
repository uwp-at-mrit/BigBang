#pragma once

#include <ppltasks.h>
#include <mutex>
#include <queue>
#include <map>

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	template<class FileType, typename Hint>
	private class IMsAppxlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		virtual ~IMsAppxlet() noexcept {
			this->shared_task.cancel();
		}

	protected:
		virtual void on_appx(Windows::Foundation::Uri^ ms_appx, FileType^ instance, Hint hint) = 0;
		virtual void on_appx_not_found(Windows::Foundation::Uri^ ms_appx, Hint hint) {
			this->get_logger()->log_message(WarGrey::SCADA::Log::Error,
				L"failed to	load %s: file does not exist",
				ms_appx->ToString()->Data());
		}

	protected:
		virtual Windows::Foundation::IAsyncOperation<FileType^>^ read(Windows::Storage::Streams::IRandomAccessStream^ stream) {
			return FileType::LoadAsync(Microsoft::Graphics::Canvas::CanvasDevice::GetSharedDevice(), stream);
		}

	protected:
		void load(Windows::Foundation::Uri^ ms_appx, Hint hint, Platform::String^ file_type = "graphlet source") {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppxlet<FileType, Hint>::critical_sections[uuid].lock();
			auto reference = IMsAppxlet<FileType, Hint>::refcounts.find(uuid);

			if (reference == IMsAppxlet<FileType, Hint>::refcounts.end()) {
				IMsAppxlet<FileType, Hint>::refcounts[uuid] = 0;
				IMsAppxlet<FileType, Hint>::queues[uuid].push(this);
				this->load_async(uuid, ms_appx, hint, file_type);
			} else if (reference->second > 0) {
				reference->second += 1;
				this->on_appx(ms_appx, IMsAppxlet<FileType, Hint>::filesystem[uuid], hint);

				this->get_logger()->log_message(Log::Debug, L"reused the %s: %s with reference count %d",
					file_type->Data(), ms_appx->ToString()->Data(),
					IMsAppxlet<FileType, Hint>::refcounts[uuid]);
			} else {
				IMsAppxlet<FileType, Hint>::queues[uuid].push(this);
				this->get_logger()->log_message(Log::Debug, L"waiting for the %s: %s",
					file_type->Data(), ms_appx->ToString()->Data());
			}
			IMsAppxlet<FileType, Hint>::critical_sections[uuid].unlock();
		}

		void unload(Windows::Foundation::Uri^ ms_appx) {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppxlet<FileType, Hint>::critical_sections[uuid].lock();
			auto reference = IMsAppxlet<FileType, Hint>::refcounts.find(uuid);

			if (reference != IMsAppxlet<FileType, Hint>::refcounts.end()) {
				if (reference->second <= 1) {
					IMsAppxlet<FileType, Hint>::refcounts.erase(uuid);
					IMsAppxlet<FileType, Hint>::filesystem.erase(uuid);
				} else {
					reference->second -= 1;
				}
			}
			IMsAppxlet<FileType, Hint>::critical_sections[uuid].unlock();
		}

	private:
		void load_async(int uuid, Windows::Foundation::Uri^ ms_appx, Hint hint, Platform::String^ file_type) {
			auto token = this->shared_task.get_token();
			auto get_file = Concurrency::create_task(Windows::Storage::StorageFile::GetFileFromApplicationUriAsync(ms_appx), token);

			get_file.then([=](Concurrency::task<Windows::Storage::StorageFile^> file) {
				return Concurrency::create_task(file.get()->OpenAsync(Windows::Storage::FileAccessMode::Read,
					Windows::Storage::StorageOpenOptions::AllowOnlyReaders),
					token);
			}).then([=](Concurrency::task<Windows::Storage::Streams::IRandomAccessStream^> stream) {
				this->get_logger()->log_message(Log::Debug, L"found the %s: %s",
					file_type->Data(), ms_appx->ToString()->Data());

				return Concurrency::create_task(this->read(stream.get()), token);
			}).then([=](Concurrency::task<FileType^> doc) {
				IMsAppxlet<FileType, Hint>::critical_sections[uuid].lock();
				try {
					FileType^ fsobject = doc.get();
					std::queue<IMsAppxlet<FileType, Hint>*> q = IMsAppxlet<FileType, Hint>::queues[uuid];

					IMsAppxlet<FileType, Hint>::filesystem[uuid] = fsobject;

					while (!q.empty()) {
						auto self = q.front();

						self->on_appx(ms_appx, fsobject, hint);
						self->info->master->notify_graphlet_ready(self);
						IMsAppxlet<FileType, Hint>::refcounts[uuid] += 1;
						q.pop();
					}

					this->get_logger()->log_message(Log::Debug, L"loaded the %s: %s with reference count %d",
						file_type->Data(), ms_appx->ToString()->Data(),
						IMsAppxlet<FileType, Hint>::refcounts[uuid]);
					IMsAppxlet<FileType, Hint>::queues.erase(uuid);
				} catch (Platform::Exception^ e) {
					IMsAppxlet<FileType, Hint>::clear(uuid);

					if (e->HResult == 0x80070002) {
						this->on_appx_not_found(ms_appx, hint);
					} else {
						this->get_logger()->log_message(WarGrey::SCADA::Log::Error, L"failed to	load %s: %s",
							ms_appx->ToString()->Data(), e->Message->Data());
					}
				} catch (Concurrency::task_canceled&) {
					IMsAppxlet<FileType, Hint>::clear(uuid);
					this->get_logger()->log_message(WarGrey::SCADA::Log::Debug,
						L"cancelled loading %s", ms_appx->ToString()->Data());
				} catch (std::exception& e) {
					IMsAppxlet<FileType, Hint>::clear(uuid);
					this->get_logger()->log_message(WarGrey::SCADA::Log::Debug,
						L"unexcepted exception: %s", e.what());
				}
				IMsAppxlet<FileType, Hint>::critical_sections[uuid].unlock();
			});
		}

	private:
		static void clear(int uuid) {
			std::queue<IMsAppxlet<FileType, Hint>*> q = IMsAppxlet<FileType, Hint>::queues[uuid];

			while (!q.empty()) {
				q.pop();
			}

			IMsAppxlet<FileType, Hint>::refcounts.erase(uuid);
			IMsAppxlet<FileType, Hint>::queues.erase(uuid);
		}

	private:
		Concurrency::cancellation_token_source shared_task;

	private:
		static std::map<int, size_t> refcounts;
		static std::map<int, FileType^> filesystem;
		static std::map<int, std::queue<IMsAppxlet<FileType, Hint>*>> queues;
		static std::map<int, std::mutex> critical_sections;
	};

	template<class FileType, typename Hint> std::map<int, size_t> IMsAppxlet<FileType, Hint>::refcounts;
	template<class FileType, typename Hint> std::map<int, FileType^> IMsAppxlet<FileType, Hint>::filesystem;
	template<class FileType, typename Hint> std::map<int, std::queue<IMsAppxlet<FileType, Hint>*>> IMsAppxlet<FileType, Hint>::queues;
	template<class FileType, typename Hint> std::map<int, std::mutex> IMsAppxlet<FileType, Hint>::critical_sections;
}
