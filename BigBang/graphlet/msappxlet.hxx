#pragma once

#include "msappx.hxx"

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	template<class FileType, typename Hint>
	private class IMsAppxlet abstract : public virtual WarGrey::SCADA::IMsAppx<FileType, Hint>, public virtual WarGrey::SCADA::IGraphlet {
	protected:
		void on_appx_notify(Windows::Foundation::Uri^ ms_appx, FileType^ instance, Hint hint) override {
			this->info->master->notify_graphlet_ready(this);
		}

	protected:
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message) override {
			return this->get_logger()->log_message(level, message);
		}
	};
}
