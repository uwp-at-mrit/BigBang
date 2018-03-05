#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "sprite.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    #define SNIPS_ARITY(a) (sizeof(a) / sizeof(ISnip*))

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
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		virtual void own_caret(bool is_own) {}

    public:
        ISnipInfo* info;
    };

	template<typename T>
	private class IScaleSnip : public WarGrey::SCADA::ISnip {
	public:
		T get_scale() {
			return this->scale;
		}
		
		void set_scale(T scale, bool force_update = false) {
			if ((this->scale != scale) || force_update) {
				this->scale = scale;
				this->update_scale();
			}
		}
		
	protected:
		virtual void update_scale() = 0;

	protected:
		T scale;
	};

	private class IPipeSnip : public WarGrey::SCADA::ISnip {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};

	/************************************************************************************************/
	Windows::Foundation::Rect snip_enclosing_box(
		WarGrey::SCADA::ISnip* snip, float x, float y,
		Windows::Foundation::Numerics::float3x2 tf);

	void pipe_connecting_position(
		WarGrey::SCADA::IPipeSnip* prev, WarGrey::SCADA::IPipeSnip* pipe,
		float* x, float* y, double factor_x = 0.5, double factor_y = 0.5);

	Windows::Foundation::Numerics::float2 pipe_connecting_position(
		WarGrey::SCADA::IPipeSnip* prev, WarGrey::SCADA::IPipeSnip* pipe,
		float x = 0.0F, float y = 0.0F, double factor_x = 0.5, double factor_y = 0.5);
}
