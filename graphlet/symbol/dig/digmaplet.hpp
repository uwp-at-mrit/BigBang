#pragma once

#include "graphlet/filesystem/diglet.hpp"

namespace WarGrey::SCADA {
	private class DigMaplet : public WarGrey::SCADA::IGraphlet {
	public:
		DigMaplet(WarGrey::SCADA::DigMap^ map, double width, double height, double fontsize_times = 2.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;

	public:
		Windows::Foundation::Numerics::float2 local_to_position(float x, float y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Numerics::float2 position_to_local(double x, double y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Size length_to_local(double width, double height = 0.0);

	public:
		double scale();

	private:
		void preshape(WarGrey::SCADA::IDigDatum* dig);
		void scale_transform(double stimes, float anchor_x, float anchor_y);
		void center();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ plainfont;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> plaintexts;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> depthtexts;
		std::map<WarGrey::SCADA::FontTextDig*, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> fonttexts;

	private:
		WarGrey::SCADA::DigMap^ map;
		double geo_x;
		double geo_y;
		double geo_width;
		double geo_height;
		float width;
		float height;

	private:
		float xtranslation;
		float ytranslation;
		float tstep;
		double stimes;
		double fstimes;
		double _scale;
	};
}
