#pragma once

#include "universe.hpp"

#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
#include "snip/liquidlet.hpp"
#include "snip/pipeline/funnellet.hpp"
#include "snip/pipeline/pipelet.hpp"
#include "snip/pipeline/screwlet.hpp"
#include "snip/pipeline/gluecleanerlet.hpp"

namespace WarGrey::SCADA {
	private enum BMotor { Funnel = 0, Master, Cleaner, Slave, Count };

	private class BSegment : public WarGrey::SCADA::Universe {
	public:
		BSegment(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ caption);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args, float width, float height) override;
		void reflow(float width, float height) override;

	// never deletes these snips mannually
	private:
		Statuslet * statusbar;
		Snip* icons[1];
		Gaugelet* gauges[BMotor::Count];

	private:
		Screwlet * master;
		Screwlet* slave;
		GlueCleanerlet* cleaner;
		Funnellet* funnel;
		Vibratorlet* vibrator;
		Pipelet* pipes_1st[4];
		Pipelet* pipes_2nd[2];
		Motorlet* motors[BMotor::Count];
		Liquidlet* oil_pipes[5];
		Liquidlet* water_pipes[5];

	private:
		Platform::String^ caption;
	};
}
