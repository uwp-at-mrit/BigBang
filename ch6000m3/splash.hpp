#pragma once

#include "planet.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class SplashScreen : public WarGrey::SCADA::Planet {
	public:
		virtual ~SplashScreen() noexcept;
		SplashScreen(float square_size);
		SplashScreen(float wide_width, float wide_height);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	private:
		float logo_width;
		float logo_height;
	};
}
