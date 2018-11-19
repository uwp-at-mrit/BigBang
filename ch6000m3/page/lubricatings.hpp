#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class LubricatingsPage : public WarGrey::SCADA::Planet {
	public:
		~LubricatingsPage() noexcept;
		LubricatingsPage(WarGrey::SCADA::PLCMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		void on_tap_selected(IGraphlet* g, float local_x, float local_y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* ps_dashboard;
		WarGrey::SCADA::PLCConfirmation* sb_dashboard;
		Windows::UI::Xaml::Controls::MenuFlyout^ ps_unit_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ sb_unit_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ps_gearbox_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ sb_gearbox_op;

	private: // never deletes these graphlets mannually
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
	};
}
