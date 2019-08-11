#pragma once

#include <shared_mutex>

#include "virtualization/screen.hpp"

#include "display.hxx"
#include "forward.hpp"

namespace WarGrey::SCADA {
	private class Pasteboard : public WarGrey::SCADA::IScreen {
	public:
		Pasteboard(WarGrey::SCADA::IDisplay^ display,
			WarGrey::SCADA::DisplayFit mode = DisplayFit::None,
			float dest_width = 0.0F, float dest_height = 0.0F,
			float src_width = 0.0F, float src_height = 0.0F);

	public:
		WarGrey::SCADA::Syslog* get_logger() override;
		WarGrey::SCADA::IDisplay^ display() override;
		
		float actual_width(IPlanet* p) override;
		float actual_height(IPlanet* p) override;

		void view_resize(float width, float height) override;
		float view_width() override;
		float view_height() override;

		void min_resize(float width, float height) override;
		float min_width() override;
		float min_height() override;

	public:
		Windows::Foundation::Point global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F) override;
		Windows::Foundation::Point local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F) override;
		
	public:
		bool surface_ready() override;
		bool ui_thread_ready() override;
		bool shown() override;

	public:
		void begin_update_sequence() override;
		bool in_update_sequence() override;
		void end_update_sequence() override;
		void notify_graphlet_updated(ISprite* g) override;

	public:
		void enter_critical_section() override;
		void enter_shared_section() override;
		void leave_critical_section() override;
		void leave_shared_section() override;

	public:
		void refresh(IPlanet* target) override;

	private:
		WarGrey::SCADA::IDisplay^ _display;

	private:
		std::shared_mutex section;
		int update_sequence_depth;
		bool needs_update;
    };
}
