#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Gaugelet : public WarGrey::SCADA::Snip {
    public:
        Gaugelet(Platform::String^ caption, int ampere , int rpm, unsigned char step = 10,
            Windows::UI::Color& ampere_color = Windows::UI::Colors::RoyalBlue,
            Windows::UI::Color& rpm_color = Windows::UI::Colors::Green);

	public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_ampere(float ampere, bool force_update = false);
		void set_rpm(int rpm, bool force_update = false);

    private:
        void initialize_meters();
        void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float cscale, int mscale,
            Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scales,
            Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scale,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ scale_marks;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caption;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Alabel;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Rlabel;
        Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Ascales;
        Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Rscales;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ Acolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ Rcolor;
        unsigned int Ampere;
        unsigned int RPM;
        unsigned char step;

    private:
        float width;
        float height;
        float meter_width;
        float meter_gapsize;
        float mark_width;
        float mark_interval;

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Alayout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ Rlayout;
		float ampere;
		float rpm;
    };
}
