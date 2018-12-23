#pragma once

#include <list>

#include "forward.hpp"

namespace WarGrey::SCADA {
	private interface class IUniverseNavigatorListener {
		void on_navigate(int from_index, int to_index);
	};

	private class IUniverseNavigator abstract {
	public:
		virtual void insert(WarGrey::SCADA::IPlanet* planet) = 0;
		virtual void select(WarGrey::SCADA::IPlanet* planet) = 0;
		virtual int selected_index() = 0;

	public:
		virtual Windows::UI::Xaml::Controls::Control^ user_interface() = 0;

	public:
		void append_navigation_listener(IUniverseNavigatorListener^ actor);
		void navigate(int from_index, int to_index);

	public:
		float min_width();
		void min_width(double v);

		float min_height();
		void min_height(double v);

	private:
		std::list<IUniverseNavigatorListener^> listeners;
	};
}
