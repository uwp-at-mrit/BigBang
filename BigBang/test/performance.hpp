#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "graphlet/dashboard/timeserieslet.hpp"

namespace WarGrey::SCADA {
	private enum class PerformanceItem { Send, Receive, Confirm, Render, _ };

	private class PerformancePage : public WarGrey::SCADA::Planet, public WarGrey::SCADA::IPLCStatusListener {
	public:
		PerformancePage(WarGrey::SCADA::IPLCMaster* plc, double timeout_ms = 800.0,
			unsigned int step = 4U, unsigned int precision = 1U);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
		void on_elapse(long long count, long long interval, long long uptime, long long elapsed) override;

	public:
		bool can_select_multiple() override;
		void on_focus(IGraphlet* g) override;
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& points, float x, float y) override;

	public:
		void on_send_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) override;
		void on_receive_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) override;
		void on_confirm_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) override;

	private:
		WarGrey::SCADA::IPLCMaster* device;
		double timeout;
		unsigned int step;
		unsigned int precision;

	private: // never this graphlet manually
		TimeSerieslet<WarGrey::SCADA::PerformanceItem>* timeseries;
	};
}
