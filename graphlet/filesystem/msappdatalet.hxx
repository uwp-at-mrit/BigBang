#pragma once

#include "filesystem/msappdata.hxx"

namespace WarGrey::SCADA {
	template<class FileType, class Graphlet>
	private class IMsAppdatalet abstract : public virtual WarGrey::SCADA::IMsAppdata<FileType>, public virtual Graphlet {
	protected:
		void on_appdata_notify(Windows::Foundation::Uri^ ms_appdata, FileType^ instance) override {
			this->notify_ready();
		}

	protected:
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->get_logger()->log_message(level, message);
		}
	};
}
