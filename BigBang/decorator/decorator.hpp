#pragma once

#include "forward.hpp"
#include "object.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class IPlanetDecorator abstract {
	public:
		virtual ~IPlanetDecorator() noexcept {}

	public:
		float actual_width();
		float actual_height();
		void fill_graphlets_boundary(float* x, float* y, float* width, float* height);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);
		WarGrey::SCADA::Syslog* get_logger();

	public:
		virtual void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) {}
		virtual void draw_after(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) {}

		virtual void draw_before_graphlet(
			WarGrey::SCADA::IGraphlet* g,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) {}

		virtual void draw_after_graphlet(
			WarGrey::SCADA::IGraphlet* g,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) {}

	public:
		void set_active_planet(IPlanet* master);

	public:
		virtual void update(long long count, long long interval, long long uptime) {}

	private:
		IPlanet* master;
	};
}
	