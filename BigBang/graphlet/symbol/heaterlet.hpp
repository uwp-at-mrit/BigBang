#pragma once
#pragma warning(disable: 4250)

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class HeaterState {
		Default,
		Running, Starting, Unstartable,
		Stopped, Stopping, Unstoppable,
		Ready, Broken,
		_
	};

	private struct HeaterStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ thread_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ auto_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ remote_color;
	};

	private class Heaterlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::HeaterState, WarGrey::SCADA::HeaterStyle> {
	public:
		Heaterlet(WarGrey::SCADA::HeaterState default_state, float radius, double degrees = 0.0);
		Heaterlet(float radius, double degrees = 0.0);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_remote_control(bool on);
		void set_auto_mode(bool yes);

	protected:
		void prepare_style(WarGrey::SCADA::HeaterState status, WarGrey::SCADA::HeaterStyle& style) override;
		void on_state_changed(WarGrey::SCADA::HeaterState state) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ thread;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ border;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ thread_color;

	private:
		bool remote_control;
		bool auto_mode;

	private:
		float brdiff;
	};
}
