#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "sprite.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    private class ISnipInfo abstract {
    public:
		virtual ~ISnipInfo() noexcept {};
		ISnipInfo(IPlanet* master) : master(master) {};
		
    public:
		IPlanet* master;
    };

	private class ISnip abstract : public WarGrey::SCADA::ISprite {
    public:
		virtual ~ISnip() noexcept;

	public:
		// `id` is designed for user-applications, in order to distinguish instances of a snip class.
		// User-Applications should define and maintain the enumerations on their own.
		long int id = -1L;
        
	public:
		virtual void own_caret(bool is_own) {}

    public:
        ISnipInfo* info;
    };

	private class IPipeSnip : public WarGrey::SCADA::ISnip {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};
}
