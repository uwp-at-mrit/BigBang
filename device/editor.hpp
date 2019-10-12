#pragma once

#include "planet.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/buttonlet.hpp"

namespace WarGrey::SCADA {
	private class EditorPlanet : public WarGrey::SCADA::Planet {
	public:
		EditorPlanet(Platform::String^ caption, unsigned int initial_mode = 0);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
		
	public:
		bool can_select(WarGrey::SCADA::IGraphlet* g) override;
		void on_tap_selected(IGraphlet* g, float local_x, float local_y) override;
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		void on_focus(IGraphlet* g, bool yes) override;

	protected:
		virtual void on_apply() = 0;
		virtual void on_reset() = 0;
		virtual bool on_discard() { return true; }
		virtual bool on_edit(WarGrey::SCADA::Dimensionlet* dim) { return true; }

	protected:
		void notify_modification();

	protected: // never delete these graphlets manually
		WarGrey::SCADA::Labellet* caption;
		WarGrey::SCADA::Buttonlet* apply;
		WarGrey::SCADA::Buttonlet* reset;
		WarGrey::SCADA::Buttonlet* discard;
		WarGrey::SCADA::Shapelet* background;
	};
}
