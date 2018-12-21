#pragma once

#include <list>

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private ref class ITimerListener abstract {
	public:
		virtual void on_elapsed(long long count, long long interval, long long uptime) = 0;
		virtual void on_elapsed(long long count, long long interval, long long uptime, long long span) {}

	internal:
		virtual WarGrey::SCADA::Syslog* get_logger() = 0;
	};

	private ref class Timer sealed {
	public:
		virtual ~Timer();
		Timer(WarGrey::SCADA::ITimerListener^ callback_executor, int rate = 60);

	public:
		void start();
		void stop();

	private:
		void notify(Platform::Object^ whocares, Platform::Object^ useless);

	private:
		Windows::UI::Xaml::DispatcherTimer^ timer;
		WarGrey::SCADA::ITimerListener^ target;

	private:
		long long count;
		long long interval;
		long long uptime;
	};

	private ref class CompositeTimerListener : public WarGrey::SCADA::ITimerListener {
	public:
		void on_elapsed(long long count, long long interval, long long uptime) override;
		void on_elapsed(long long count, long long interval, long long uptime, long long span) override;

	public:
		void append_timer_action(WarGrey::SCADA::ITimerListener^ receiver);

	internal:
		WarGrey::SCADA::Syslog* get_logger() override;

	private:
		std::list<ITimerListener^> listeners;
	};
}
