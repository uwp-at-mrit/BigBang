#pragma once

#include "graphlet/primitive.hpp"
#include "virtualization/screen.hpp"
#include "forward.hpp"

namespace WarGrey::SCADA {
	private class Planetlet : public WarGrey::SCADA::IGraphlet {
	public:
		virtual ~Planetlet() noexcept;
		Planetlet(WarGrey::SCADA::IPlanet* planet, float width, float height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;
		void on_hover(float local_x, float local_y) override;
		void on_tap(float local_x, float local_y) override;
		void on_goodbye(float local_x, float local_y) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;

	private:
		WarGrey::SCADA::IScreen* screen;
		WarGrey::SCADA::IPlanet* planet;
		float width;
		float height;
	};
}
