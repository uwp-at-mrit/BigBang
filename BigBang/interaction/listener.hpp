#pragma once

#include "object.hpp"
#include "forward.hpp"

namespace WarGrey::SCADA {
	private class IUniverseListener abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool can_move(IPlanet* master, ISnip* snip) { return false; }
		virtual bool can_select(IPlanet* master, ISnip* snip, float x, float y) { return false; }
		virtual bool can_select_multiple(IPlanet* master) { return false; }

	public:
		virtual void before_select(IPlanet* master, ISnip* snip, bool on_or_off, float x, float y) {}
		virtual void after_select(IPlanet* master, ISnip* snip, bool on_or_off, float x, float y) {}
	};
}
