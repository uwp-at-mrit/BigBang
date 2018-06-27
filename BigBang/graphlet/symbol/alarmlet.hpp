#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class AlarmStatus { Normal, Alert, _ };

	private struct AlarmStyle {
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color;
	};

	private class Alarmlet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::AlarmStatus, WarGrey::SCADA::AlarmStyle> {
    public:
        Alarmlet(float size);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
        float width;
		float height;
    };
}
