#pragma once

#include "planet.hpp"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private enum class DragView { PortSide, Starboard, Suctions, _ };

	private class DredgesPage : public WarGrey::SCADA::Planet {
	public:
		virtual ~DredgesPage() noexcept;
		DredgesPage(WarGrey::SCADA::PLCMaster* plc, WarGrey::SCADA::DragView type = WarGrey::SCADA::DragView::_);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		bool can_select_multiple() override;
		void on_focus(IGraphlet* g) override;
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		void on_tap_selected(IGraphlet* g, float x, float y) override;
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& anchors, float x, float y);

	public:
		WarGrey::SCADA::PLCMaster* get_plc_device();

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
	};
}
