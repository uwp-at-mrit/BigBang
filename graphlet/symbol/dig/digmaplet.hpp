#pragma once

#include "graphlet/matrix/diglet.hpp"

namespace WarGrey::SCADA {
	private class DigMaplet : public WarGrey::SCADA::IGraphlet {
	public:
		DigMaplet(WarGrey::SCADA::DigMap^ map, double width, double height, double tx, double ty);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 local_to_position(float x, float y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Numerics::float2 position_to_local(double x, double y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Size length_to_local(double width, double height = 0.0);

	private:
		void draw_text(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Platform::Object^ vtobj, Windows::Foundation::Numerics::float2& tp, long long color,
			WarGrey::SCADA::IDigDatum* dig, float lx, float ty, float rx, float by);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ plainfont;
		std::map<Platform::String^, Platform::Object^> plaintexts;
		std::map<WarGrey::SCADA::FontTextDig*, Platform::Object^> fonttexts;

	private:
		WarGrey::SCADA::DigMap^ map;
		double initial_width;
		double initial_height;
		float width;
		float height;

	private:
		double xscale;
		double yscale;
		double xtranslation;
		double ytranslation;
	};
}
