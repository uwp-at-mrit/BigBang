#include "timer.hxx"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;

Timer::Timer(ITimerListener^ callback, int rate, unsigned int shift) : target(callback), interval(0LL) {
	this->timer = ref new DispatcherTimer();

	this->timer->Tick += ref new EventHandler<Platform::Object^>(this, &Timer::notify);

	if (rate != 0) {
		// NOTE: the first notification will occur after the first interval, not occurs immediately.
		this->start(rate, shift);
	}
}

Timer::~Timer() {
	this->stop();
}

void Timer::start(int rate, unsigned int shift) {
	if (rate != 0) {
		this->set_frame_rate(rate, shift);
	} else if (this->interval == 0LL) {
		this->set_frame_rate(60, shift);
	}

	this->timer->Start();

	this->count = 1;
	this->uptime = this->interval;
}

void Timer::stop() {
	this->timer->Stop();
}

void Timer::notify(Platform::Object^ whocares, Platform::Object^ useless) {
	// TODO: meanwhile the next round will elapse after this one finished. 
	long long elapsed0 = current_100nanoseconds();
	long long elapsed, next_tick;

	this->target->on_elapse(this->count, this->interval, this->uptime);
	elapsed = current_100nanoseconds() - elapsed0;
	next_tick = this->interval - elapsed;
	this->target->on_elapse(this->count, this->interval, this->uptime, elapsed);
	
	this->count += 1;
	this->uptime += this->interval;

	while (next_tick < 0) {
		next_tick += this->interval;

		this->count += 1;
		this->uptime += this->interval;
	}

	this->timer->Interval = TimeSpan{ next_tick }; // don't worry, it's Visual Studio's fault
}

void Timer::set_frame_rate(int rate, unsigned int shift) {
	this->timer->Interval = make_timespan_from_rate(rate, shift);
	this->interval = this->timer->Interval.Duration;
}

/*************************************************************************************************/
void CompositeTimerListener::on_elapse(long long count, long long interval, long long uptime) {
	for (auto action : this->listeners) {
		action->on_elapse(count, interval, uptime);
	}
}

void CompositeTimerListener::on_elapse(long long count, long long interval, long long uptime, long long elapsed) {
	for (auto action : this->listeners) {
		action->on_elapse(count, interval, uptime, elapsed);
	}
}

void CompositeTimerListener::push_timer_listener(ITimerListener^ action) {
	this->listeners.push_back(action);
}

Syslog* CompositeTimerListener::get_logger() {
	Syslog* logger = default_logger();
	auto maybe_action = this->listeners.begin();

	if (maybe_action != this->listeners.end()) {
		logger = (*maybe_action)->get_logger();
	}

	return logger;
}
