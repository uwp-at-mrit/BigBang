#pragma once

#include "graphlet/shapelet.hpp"

#include "graphlet/ui/textlet.hpp"
#include "graphlet/ui/buttonlet.hpp"

#include "datum/credit.hpp"

#include "palette.hpp"
#include "satellite.hpp"

namespace WarGrey::DTPM {
	private class IColorPickerReceiver abstract {
	public:
		virtual void on_color_pick(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color, WarGrey::SCADA::IGraphlet* target) = 0;
	};

	private class ColorPicker : public WarGrey::SCADA::ISatellite {
	public:
		static WarGrey::DTPM::ColorPicker* get_instance(WarGrey::DTPM::Palette type);

	public:
		void fill_extent(float* width, float* height) override;

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(WarGrey::SCADA::IGraphlet* g) override;
		void on_tap_selected(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) override;

	public: // learn C++ "Name Hiding"
		void show(WarGrey::DTPM::IColorPickerReceiver* receiver, WarGrey::SCADA::IGraphlet* target); // this will hide `ISatellite::show`;
		void on_hiden() override;

	private:
		~ColorPicker() noexcept;
		ColorPicker(IPalette* palette);

	private: // never delete these graphlets manually.
		WarGrey::SCADA::Credit<WarGrey::SCADA::Rectanglet, unsigned int>** colors;
		WarGrey::SCADA::Labellet* label;
		WarGrey::SCADA::Buttonlet* okay;
		WarGrey::SCADA::Buttonlet* cancel;

	private:
		WarGrey::DTPM::IPalette* palette;

	private:
		WarGrey::SCADA::Credit<WarGrey::SCADA::Rectanglet, unsigned int>* selected_color;
		WarGrey::DTPM::IColorPickerReceiver* receiver;
		WarGrey::SCADA::IGraphlet* target;

	private:
		float gapsize;
	};
}
