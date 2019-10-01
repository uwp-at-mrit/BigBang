#pragma once

#include "filesystem/msappdatalogue.hxx"

namespace WarGrey::SCADA {
	template<class FileType, class Graphlet, typename TypeName>
	private class IMsAppdataLoguelet abstract : public virtual WarGrey::SCADA::IMsAppdataLogue<FileType, TypeName>, public virtual Graphlet {
	protected:
		void on_appdata_notify(Platform::String^ file, FileType^ instance, TypeName type) override {
			this->notify_ready();
		}

		void on_directory_changed(Windows::Storage::StorageFolder^ rootdir) override {
			this->list_files();
		}

	protected:
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->get_logger()->log_message(level, message);
		}
	};
}
