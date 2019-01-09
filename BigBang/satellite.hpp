#pragma once

#include "planet.hpp"
#include "syslog.hpp"

#include "hamburger.hpp"

namespace WarGrey::SCADA {
	private class ISatellite abstract : public WarGrey::SCADA::IHamburger, public WarGrey::SCADA::Planet {
	public:
		ISatellite(WarGrey::SCADA::Syslog* logger, Platform::String^ caption, unsigned int initial_mode = 0U);
		ISatellite(WarGrey::SCADA::Log level, Platform::String^ caption, unsigned int initial_mode = 0U);
		
	public:
		void notify_surface_ready() override;
		bool surface_ready() override;

	protected:
		virtual void on_surface_ready() {}

	protected:
		Windows::UI::Xaml::Controls::Flyout^ user_interface() override;

	private:
		Windows::UI::Xaml::Controls::Flyout^ orbit;
		bool ready;
	};

	template<typename ID>
	private class ICreditSatellite abstract : public WarGrey::SCADA::ISatellite {
	public:
		ICreditSatellite(WarGrey::SCADA::Syslog* logger, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(logger, caption, initial_mode), pending(true) {}

		ICreditSatellite(WarGrey::SCADA::Log level, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(level, caption, initial_mode), pending(true) {}

	public:
		ID get_id() {
			return this->channel;
		}

		void switch_id(ID id) {
			if (id != this->channel) {
				this->channel = id;

				if (this->surface_ready()) {
					this->begin_update_sequence();
					this->on_id_changed(this->channel);
					this->end_update_sequence();
				} else {
					this->pending = true;
				}
			}
		}

	protected:
		void on_surface_ready() override {
			if (this->pending) {
				this->pending = false;

				this->begin_update_sequence();
				this->on_id_changed(this->channel);
				this->end_update_sequence();
			}
		}

	protected:
		virtual void on_id_changed(ID channel) = 0;

	protected:
		ID channel;

	private:
		bool pending;
	};
}
