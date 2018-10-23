#pragma once

#include "planet.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class Gallery : public WarGrey::SCADA::Planet {
	public:
		~Gallery() noexcept;
		Gallery();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void update(long long count, long long interval, long long uptime) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(WarGrey::SCADA::IGraphlet* g) override;

	private: // never deletes these graphlets mannually	
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
	};
}
