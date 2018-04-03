#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private ref class ITimerAction abstract {
	public:
		virtual void on_elapsed(long long count, long long interval, long long uptime) = 0;

	internal:
		virtual WarGrey::SCADA::Syslog* get_logger() = 0;
	};

	private ref class Timer sealed {
	public:
		virtual ~Timer();
		Timer(WarGrey::SCADA::ITimerAction^ callback_executor, int rate = 60);

	public:
		void start();
		void stop();

	private:
		void notify(Platform::Object^ whocares, Platform::Object^ useless);

	private:
		Windows::UI::Xaml::DispatcherTimer^ timer;
		Windows::Globalization::Calendar^ calendar;
		WarGrey::SCADA::ITimerAction^ target;

	private:
		long long count;
		long long interval;
		long long uptime;
	};
}