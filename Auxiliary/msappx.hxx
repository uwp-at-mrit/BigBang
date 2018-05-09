#pragma once

#include <ppltasks.h>
#include <mutex>
#include <queue>
#include <map>

#include "syslog.hpp"
#include "string.hpp"

namespace WarGrey::SCADA {
	template<class FileType, typename Hint>
	private class IMsAppx abstract {
	public:
		virtual ~IMsAppx() noexcept {
			this->shared_task.cancel();
		}

	protected:
		virtual void on_appx(Windows::Foundation::Uri^ ms_appx, FileType^ ftobject, Hint hint) = 0;

		virtual void on_appx_notify(Windows::Foundation::Uri^ ms_appx, FileType^ ftobject, Hint hint) {}
		
		virtual void on_appx_not_found(Windows::Foundation::Uri^ ms_appx, Hint hint) {
			this->log_message(WarGrey::SCADA::Log::Error,
				make_string(L"failed to load %s: file does not exist",
					ms_appx->ToString()->Data()));
		}

	protected:
		virtual void log_message(WarGrey::SCADA::Log level, Platform::String^ message) {
			syslog(level, message);
		}

		virtual Windows::Foundation::IAsyncOperation<FileType^>^ read(Windows::Storage::Streams::IRandomAccessStream^ stream) {
			return FileType::LoadAsync(Microsoft::Graphics::Canvas::CanvasDevice::GetSharedDevice(), stream);
		}

	protected:
		void load(Windows::Foundation::Uri^ ms_appx, Hint hint, Platform::String^ file_type = "graphlet source") {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppx<FileType, Hint>::critical_sections[uuid].lock();
			auto reference = IMsAppx<FileType, Hint>::refcounts.find(uuid);

			if (reference == IMsAppx<FileType, Hint>::refcounts.end()) {
				IMsAppx<FileType, Hint>::refcounts[uuid] = 0;
				IMsAppx<FileType, Hint>::queues[uuid].push(this);
				this->load_async(uuid, ms_appx, hint, file_type);
			} else if (reference->second > 0) {
				reference->second += 1;
				this->on_appx(ms_appx, IMsAppx<FileType, Hint>::filesystem[uuid], hint);

				this->log_message(Log::Debug, 
					make_string(L"reused the %s: %s with reference count %d",
						file_type->Data(), ms_appx->ToString()->Data(),
						IMsAppx<FileType, Hint>::refcounts[uuid]));
			} else {
				IMsAppx<FileType, Hint>::queues[uuid].push(this);
				this->log_message(Log::Debug,
					make_string(L"waiting for the %s: %s",
						file_type->Data(), ms_appx->ToString()->Data()));
			}
			IMsAppx<FileType, Hint>::critical_sections[uuid].unlock();
		}

		void unload(Windows::Foundation::Uri^ ms_appx) {
			auto uuid = ms_appx->ToString()->GetHashCode();

			IMsAppx<FileType, Hint>::critical_sections[uuid].lock();
			auto reference = IMsAppx<FileType, Hint>::refcounts.find(uuid);

			if (reference != IMsAppx<FileType, Hint>::refcounts.end()) {
				if (reference->second <= 1) {
					IMsAppx<FileType, Hint>::refcounts.erase(uuid);
					IMsAppx<FileType, Hint>::filesystem.erase(uuid);
				} else {
					reference->second -= 1;
				}
			}
			IMsAppx<FileType, Hint>::critical_sections[uuid].unlock();
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
				this->log_message(Log::Debug,
					make_string(L"found the %s: %s",
						file_type->Data(), ms_appx->ToString()->Data()));

				return Concurrency::create_task(this->read(stream.get()), token);
			}).then([=](Concurrency::task<FileType^> doc) {
				IMsAppx<FileType, Hint>::critical_sections[uuid].lock();
				try {
					FileType^ ftobject = doc.get();
					std::queue<IMsAppx<FileType, Hint>*> q = IMsAppx<FileType, Hint>::queues[uuid];

					IMsAppx<FileType, Hint>::filesystem[uuid] = ftobject;

					while (!q.empty()) {
						auto self = q.front();

						self->on_appx(ms_appx, ftobject, hint);
						self->on_appx_notify(ms_appx, ftobject, hint);
						IMsAppx<FileType, Hint>::refcounts[uuid] += 1;
						q.pop();
					}

					this->log_message(Log::Debug,
						make_string(L"loaded the %s: %s with reference count %d",
							file_type->Data(), ms_appx->ToString()->Data(),
							IMsAppx<FileType, Hint>::refcounts[uuid]));
					IMsAppx<FileType, Hint>::queues.erase(uuid);
				} catch (Platform::Exception^ e) {
					IMsAppx<FileType, Hint>::clear(uuid);

					if (e->HResult == 0x80070002) {
						this->on_appx_not_found(ms_appx, hint);
					} else {
						this->log_message(WarGrey::SCADA::Log::Error,
							make_string(L"failed to load %s: %s",
								ms_appx->ToString()->Data(), e->Message->Data()));
					}
				} catch (Concurrency::task_canceled&) {
					IMsAppx<FileType, Hint>::clear(uuid);
					this->log_message(WarGrey::SCADA::Log::Debug,
						make_string(L"cancelled loading %s", ms_appx->ToString()->Data()));
				} catch (std::exception& e) {
					IMsAppx<FileType, Hint>::clear(uuid);
					this->log_message(WarGrey::SCADA::Log::Debug,
						make_string(L"unexcepted exception: %s", e.what()));
				}
				IMsAppx<FileType, Hint>::critical_sections[uuid].unlock();
			});
		}

	private:
		static void clear(int uuid) {
			std::queue<IMsAppx<FileType, Hint>*> q = IMsAppx<FileType, Hint>::queues[uuid];

			while (!q.empty()) {
				q.pop();
			}

			IMsAppx<FileType, Hint>::refcounts.erase(uuid);
			IMsAppx<FileType, Hint>::queues.erase(uuid);
		}

	private:
		Concurrency::cancellation_token_source shared_task;

	private:
		static std::map<int, size_t> refcounts;
		static std::map<int, FileType^> filesystem;
		static std::map<int, std::queue<IMsAppx<FileType, Hint>*>> queues;
		static std::map<int, std::mutex> critical_sections;
	};

	template<class FileType, typename Hint> std::map<int, size_t> IMsAppx<FileType, Hint>::refcounts;
	template<class FileType, typename Hint> std::map<int, FileType^> IMsAppx<FileType, Hint>::filesystem;
	template<class FileType, typename Hint> std::map<int, std::queue<IMsAppx<FileType, Hint>*>> IMsAppx<FileType, Hint>::queues;
	template<class FileType, typename Hint> std::map<int, std::mutex> IMsAppx<FileType, Hint>::critical_sections;
}
