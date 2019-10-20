#pragma once

#include "graphlet/primitive.hpp"
#include "graphlet/filesystem/project/reader/digdoc.hxx"

namespace WarGrey::SCADA {
	private enum class MapMove : unsigned int { Up, Right, Down, Left, ScaleUp, ScaleDown, Reset };

	private class DigMaplet : public WarGrey::SCADA::IGraphlet {
	public:
		DigMaplet(WarGrey::SCADA::DigDoc^ map, double width, double height, double fontsize_times = 2.0, float plain_fontsize = 14.0F);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 local_to_position(float x, float y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Numerics::float2 position_to_local(double x, double y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Size length_to_local(double width, double height = 0.0);

	public:
		void fill_anchor_position(double fx, double fy, double* x = nullptr, double* y = nullptr);
		float scaled_font_size(long long origin_fontsize);
		float plain_font_size();
		double scale();

	public:
		void transform(WarGrey::SCADA::MapMove move);
		void center();
		void center_at(double x, double y);

	private:
		void preshape(WarGrey::SCADA::IDigDatum* dig);
		void scale_transform(double stimes, float anchor_x, float anchor_y);
		
	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ plainfont;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> plaintexts;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> numtexts;
		std::map<WarGrey::SCADA::FontTextDig*, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> fonttexts;

	private:
		WarGrey::SCADA::DigDoc^ map;
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
