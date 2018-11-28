#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class AlarmState { None, Notice, Warning, Alert, _ };

	private struct AlarmStyle {
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color;
	};

	private class Alarmlet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::AlarmState, WarGrey::SCADA::AlarmStyle> {
    public:
        Alarmlet(float size);
		Alarmlet(WarGrey::SCADA::AlarmState default_state, float size);

    public:
		void construct() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	protected:
		void prepare_style(WarGrey::SCADA::AlarmState status, WarGrey::SCADA::AlarmStyle& style) override;
		void apply_style(WarGrey::SCADA::AlarmStyle& style) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

    private:
        float width;
		float height;
    };
}
