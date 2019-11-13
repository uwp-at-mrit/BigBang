#pragma once

#include "graphlet/primitive.hpp"
#include "virtualization/screen.hpp"
#include "forward.hpp"

namespace WarGrey::SCADA {
	private class Planetlet : public WarGrey::SCADA::IGraphlet {
	public:
		virtual ~Planetlet() noexcept;
		
		Planetlet(WarGrey::SCADA::IPlanet* planet, float width = 0.0F, float height = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr);

		Planetlet(WarGrey::SCADA::IPlanet* planet, WarGrey::SCADA::GraphletAnchor anchor,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void resize(float width, float height) override;

	public:
		void enable_stretch(bool resizable_width, bool resizable_height);
		void enable_stretch(bool yes_or_no);
		void set_stretch_anchor(WarGrey::SCADA::GraphletAnchor anchor);

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;

		bool on_pointer_moved(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

		bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

		bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

		bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk) override;

		bool on_pointer_wheeled(float x, float y, float delta, bool horizontal, bool controlled) override;

	protected:
		WarGrey::SCADA::IPlanet* planet;

	private:
		void triggle_resize_event_if_needed();

	private:
		WarGrey::SCADA::IScreen* screen;
		float width;
		float height;

	private:
		bool stretchable_width;
		bool stretchable_height;
		WarGrey::SCADA::GraphletAnchor stretching_anchor;
	};
}
