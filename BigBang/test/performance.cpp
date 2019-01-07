#include "test/performance.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas::UI;

/*************************************************************************************************/
PerformancePage::PerformancePage(IPLCMaster* plc, double timeout_ms, unsigned int step, unsigned int precision)
	: Planet(__MODULE__), device(plc), timeout(timeout_ms), step(step), precision(precision) {
	this->device->push_plc_status_listener(this);
}

void PerformancePage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->timeseries == nullptr) {
		float lines_width = width * 0.8F;
		float lines_height = height * 0.8F;

		this->timeseries = this->insert_one(new TimeSerieslet<PerformanceItem>(__MODULE__,
			this->timeout, make_minute_series(4U), lines_width, lines_height, this->step, this->precision));
	}
}

void PerformancePage::reflow(float width, float height) {
	if (this->timeseries != nullptr) {
		this->move_to(this->timeseries, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
	}
}

bool PerformancePage::can_select_multiple() {
	return true;
}

void PerformancePage::on_focus(IGraphlet* g) {
	if (this->timeseries == g) {
		this->show_virtual_keyboard(ScreenKeyboard::Arrowpad, this->timeseries, GraphletAnchor::CB, 0.0F, 4.0F);
	}
}

void PerformancePage::on_gesture(std::list<Windows::Foundation::Numerics::float2>& points, float x, float y) {
	//this->get_logger()->log_message(Log::Info, L"(%f, %f)", x, y);
}

void PerformancePage::on_send_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {
	this->timeseries->set_value(PerformanceItem::Send, span_ms);
}

void PerformancePage::on_receive_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {
	this->timeseries->set_value(PerformanceItem::Receive, span_ms);
}

void PerformancePage::on_confirm_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {
	this->timeseries->set_value(PerformanceItem::Confirm, span_ms);
}

void PerformancePage::on_elapse(long long count, long long interval, long long uptime, long long elapsed) {
	this->timeseries->set_value(PerformanceItem::Render, double(elapsed) / 10000.0);
}
