#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class AlarmStatus { Normal, Alert, _ };

	private struct AlarmStyle {
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color;
	};

	private class Alarmlet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::AlarmStatus, WarGrey::SCADA::AlarmStyle> {
    public:
        Alarmlet(float width);
		Alarmlet(WarGrey::SCADA::AlarmStatus default_status, float width);

    public:
		void construct() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	protected:
		void prepare_style(WarGrey::SCADA::AlarmStatus status, WarGrey::SCADA::AlarmStyle& style) override;
		void apply_style(WarGrey::SCADA::AlarmStyle& style) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

    private:
        float width;
		float height;
    };
}
