#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "decorator/grid.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class FillnJetPage : public WarGrey::SCADA::Planet {
	public:
		~FillnJetPage() noexcept;
		FillnJetPage(WarGrey::SCADA::IMRMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		void on_tap(IGraphlet* g, float x, float y, bool shifted, bool ctrled) override;

	private:
		WarGrey::SCADA::IMRMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		Windows::UI::Xaml::Controls::MenuFlyout^ operation;

	private: // never deletes these graphlets mannually
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;

	private:
		WarGrey::SCADA::GridDecorator* grid;
	};
}
