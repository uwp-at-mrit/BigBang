#include "timer.hxx"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Globalization;

using namespace Windows::UI::Xaml;

Timer::Timer(ITimerAction^ callback, int rate) : target(callback) {
	this->calendar = ref new Calendar();
	this->timer = ref new DispatcherTimer();

	this->timer->Interval = make_timespan_from_rate(rate);
	this->interval = this->timer->Interval.Duration;
	this->timer->Tick += ref new EventHandler<Platform::Object^>(this, &Timer::notify);

	// NOTE: the first notification will occur after the first interval, not occurs immediately.
	this->start();
}

Timer::~Timer() {
	this->stop();
}

void Timer::notify(Platform::Object^ whocares, Platform::Object^ useless) {
	// TODO: meanwhile the next round will elapse after this one finished. 
	long long elapsed0, elapsed, next_interval;

	this->calendar->SetToNow();
	elapsed0 = this->calendar->GetDateTime().UniversalTime;

	this->target->on_elapsed(this->count, this->interval, this->uptime);
	
	this->calendar->SetToNow();
	elapsed = this->calendar->GetDateTime().UniversalTime - elapsed0;
	next_interval = this->interval - elapsed;
	
	this->count += 1;
	this->uptime += this->interval;

	if (next_interval < 0) {
		this->target->get_logger()->log_message(Log::Notice,
			L"it took %fms to update planets, but the expected interval is %fms",
			float(elapsed) / 10000.0F, float(interval) / 10000.0F);
	} else {
		this->timer->Interval = TimeSpan{ next_interval };
		
		/* 
		this->target->get_logger()->log_message(Log::Debug,
			L"it took %fms to update planets",
			float(elapsed) / 10000.0F);
		*/
	}
}

void Timer::start() {
	this->timer->Start();

	this->count = 1;
	this->uptime = this->interval;
}

void Timer::stop() {
	this->timer->Stop();
}

/*************************************************************************************************/
void CompositeTimerAction::on_elapsed(long long count, long long interval, long long uptime) {
	for (auto action : this->actions) {
		action->on_elapsed(count, interval, uptime);
	}
}

void CompositeTimerAction::append_timer_action(ITimerAction^ action) {
	this->actions.push_back(action);
}

Syslog* CompositeTimerAction::get_logger() {
	Syslog* logger = default_logger();
	auto maybe_action = this->actions.begin();

	if (maybe_action != this->actions.end()) {
		logger = (*maybe_action)->get_logger();
	}

	return logger;
}
