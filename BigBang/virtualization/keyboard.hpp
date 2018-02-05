#pragma once

#include "forward.hpp"
#include "snip/snip.hpp"

namespace WarGrey::SCADA {
	private enum class Keyboard { Numpad };
	
	private struct KeyboardCell {
		float x;
		float y;
		float width;
		float height;
		float label_x;
		float label_y;
	};

	private class IKeyboard abstract : public WarGrey::SCADA::ISprite {
	public:
		~IKeyboard();
		IKeyboard(IPlanet* master, unsigned int keynum);
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		void construct() override;
		void show(bool shown);
		bool shown();

	public:
		virtual void fill_auto_position(float* x, float* y) = 0;
		virtual bool is_colliding_with_mouse(float mouse_x, float mouse_y, float x, float y);
		
	protected:
		virtual void create() = 0;
		virtual void fill_cell(KeyboardCell* cell, unsigned int idx) = 0;
		int find_cell(float mouse_x, float mouse_y);

	protected:
		WarGrey::SCADA::IPlanet* master;
		WarGrey::SCADA::KeyboardCell* cells;

	private:
		unsigned int keynum;
		bool _shown;
	};
}
