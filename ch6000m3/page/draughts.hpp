#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class DraughtsPage : public WarGrey::SCADA::Planet {
	public:
		~DraughtsPage() noexcept;
		DraughtsPage(WarGrey::SCADA::PLCMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
		
	public:
		bool can_select(IGraphlet* g) override;
		bool can_select_multiple() override;
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		void on_focus(IGraphlet* g) override;
		void on_tap_selected(IGraphlet* g, float local_x, float local_y) override;
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& points, float x, float y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		Windows::UI::Xaml::Controls::MenuFlyout^ overflow_op;

	private: // never deletes these graphlets mannually	
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
	};
}
