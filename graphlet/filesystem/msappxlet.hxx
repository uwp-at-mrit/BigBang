#pragma once

#include "filesystem/msappx.hxx"

namespace WarGrey::SCADA {
	template<class FileType, class Graphlet, typename Hint>
	private class IMsAppxlet abstract : public virtual WarGrey::SCADA::IMsAppx<FileType, Hint>, public virtual Graphlet {
	protected:
		void on_appx_notify(Windows::Foundation::Uri^ ms_appx, FileType^ instance, Hint hint) override {
			this->notify_ready();
		}

	protected:
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->get_logger()->log_message(level, message);
		}
	};
}
