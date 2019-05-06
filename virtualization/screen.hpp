#pragma once

#include "datum/class.hpp"

#include "forward.hpp"
#include "display.hxx"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private enum class DisplayFit { Fill, Contain, None };

    private class IScreen abstract {
	public:
		virtual ~IScreen();

	public:
		IScreen(WarGrey::SCADA::Syslog* logger,
			WarGrey::SCADA::DisplayFit mode = DisplayFit::None,
			float target_width = 0.0F, float target_height = 0.0F,
			float source_width = 0.0F, float source_height = 0.0F);

	public:
		virtual bool surface_ready() = 0;
		virtual bool ui_thread_ready() = 0;
		virtual bool shown() = 0;

	public:
		virtual WarGrey::SCADA::IDisplay^ display() = 0;
		
		virtual float actual_width(IPlanet* p) = 0;
		virtual float actual_height(IPlanet* p) = 0;

		virtual void view_resize(float width, float height) = 0;
		virtual float view_width() = 0;
		virtual float view_height() = 0;

		virtual void min_resize(float width, float height) = 0;
		virtual float min_width() = 0;
		virtual float min_height() = 0;

	public:
		virtual Windows::Foundation::Point global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F) = 0;
		virtual Windows::Foundation::Point local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F) = 0;

	public:
		void apply_source_size(float sketch_width, float sketch_height);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	public:
		virtual void refresh(IPlanet* target) = 0;

	public:
		WarGrey::SCADA::Syslog* get_logger();

	private:
		WarGrey::SCADA::Syslog* logger;

	private:
		DisplayFit mode;
		float target_width;
		float target_height;
		float source_width;
		float source_height;
    };
}
