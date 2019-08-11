#pragma once

#include "datum/class.hpp"

#include "forward.hpp"
#include "display.hxx"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private enum class DisplayFit { Fill, Contain, None };

    private class IScreen abstract {
	public:
		IScreen(WarGrey::SCADA::DisplayFit mode = DisplayFit::None,
			float target_width = 0.0F, float target_height = 0.0F,
			float source_width = 0.0F, float source_height = 0.0F);

	public:
		virtual WarGrey::SCADA::Syslog* get_logger() = 0;
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
		virtual void begin_update_sequence() = 0;
		virtual bool in_update_sequence() = 0;
		virtual void end_update_sequence() = 0;
		virtual void notify_graphlet_updated(ISprite* g) = 0;

	public:
		virtual void enter_critical_section() = 0;
		virtual void enter_shared_section() = 0;
		virtual void leave_critical_section() = 0;
		virtual void leave_shared_section() = 0;

	public:
		virtual void refresh(IPlanet* target) = 0;

	public:
		void apply_source_size(float sketch_width, float sketch_height);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	private:
		DisplayFit mode;
		float target_width;
		float target_height;
		float source_width;
		float source_height;
    };
}
