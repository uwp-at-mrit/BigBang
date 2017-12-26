#pragma once

#include "object.hpp"
#include "forward.hpp"

namespace WarGrey::SCADA {
	private class IUniverseListener abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool can_move(IUniverse* master, Snip* snip) { return false; }
		virtual bool can_select(IUniverse* master, Snip* snip) { return false; }
		virtual bool can_select_multiple(IUniverse* master) { return false; }

	public:
		virtual void before_select(IUniverse* master, Snip* snip, bool on_or_off) {}
		virtual void after_select(IUniverse* master, Snip* snip, bool on_or_off) {}
	};
}
