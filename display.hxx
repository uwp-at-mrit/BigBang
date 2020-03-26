#pragma once

#include <map>
#include <mutex>

#include "navigator/navigator.hpp"
#include "datum/class.hpp"

#include "timer.hxx"
#include "syslog.hpp"

namespace WarGrey::SCADA {
    private ref class IDisplay abstract : public WarGrey::SCADA::ITimerListener, public WarGrey::SCADA::IUniverseNavigatorListener {
	public:
		virtual ~IDisplay();

	internal:
		IDisplay(WarGrey::GYDM::Syslog* logger);

	public:
		vpure_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		vpure_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);

    public:
		virtual_read_only_property(float, actual_width);
        virtual_read_only_property(float, actual_height);
        read_write_property(float, width);
        read_write_property(float, height);
        read_write_property(float, min_width);
        read_write_property(float, min_height);
        read_write_property(float, max_width);
        read_write_property(float, max_height);

	public:
		virtual Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F) = 0;
		virtual void save(Platform::String^ path, float dpi = 96.0F);

	public:
		virtual bool surface_ready() = 0;
		virtual bool ui_thread_ready() = 0;
		virtual bool shown();

	public:
		virtual void on_navigate(int from_index, int to_index) = 0;

	public:
		void enter_critical_section();
		void leave_critical_section();

	internal:
		virtual Windows::Foundation::Point global_to_local_point(WarGrey::SCADA::IPlanet* p, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F);
		virtual Windows::Foundation::Point local_to_global_point(WarGrey::SCADA::IPlanet* p, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F);
		virtual float planet_actual_width(WarGrey::SCADA::IPlanet* p);
		virtual float planet_actual_height(WarGrey::SCADA::IPlanet* p);

	internal:
		virtual void refresh(WarGrey::SCADA::IPlanet* target) = 0;

	internal:
		WarGrey::GYDM::Syslog* get_logger() override;

	private:
		WarGrey::GYDM::Syslog* logger;
		std::mutex section;
    };
}
