#pragma once

#include "planet.hpp"
#include "graphlet/ui/textlet.hpp"

namespace WarGrey::SCADA {
	private class CyberSpace : public WarGrey::SCADA::Planet {
	public:
		virtual ~CyberSpace() noexcept;
		CyberSpace();

	public:
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;

		bool on_pointer_moved(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

		bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

	private: // never delete these graphlets manually
		WarGrey::SCADA::Labellet* hovered;
		WarGrey::SCADA::Labellet* escaped;
		WarGrey::SCADA::Labellet* keycode;
	};
}
