#pragma once

#include "mrit.hpp"

namespace WarGrey::SCADA {
	private class PLCConfiguration : public WarGrey::SCADA::IMRConfiguration {
	public:
		bool fill_signal_preferences(MRSignal type, uint16* data_block, uint16* addr0, uint16* addrn) override;

	protected:
		~PLCConfiguration() noexcept {}
	};
}
