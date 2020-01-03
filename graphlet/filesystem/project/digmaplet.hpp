#pragma once

#include "graphlet/primitive.hpp"
#include "graphlet/filesystem/project/reader/digdoc.hxx"

namespace WarGrey::DTPM {
	private enum class MapMove : unsigned int { Up, Right, Down, Left, ZoomIn, ZoomOut, Reset };

	private class DigMaplet : public WarGrey::SCADA::IGraphlet {
	public:
		DigMaplet(WarGrey::DTPM::DigDoc^ map, double width, double height, double fontsize_times = 2.0, float plain_fontsize = 14.0F);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 local_to_position(float x, float y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Numerics::float2 position_to_local(double x, double y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Size length_to_local(double width, double height = 0.0);
		Windows::Foundation::Size local_to_length(double width, double height = 0.0);

	public:
		void fill_anchor_position(double fx, double fy, double* x = nullptr, double* y = nullptr);
		float scaled_font_size(long long origin_fontsize);
		float plain_font_size();
		double actual_scale();

	public:
		void center();
		void center_at(double x, double y);
		void translate(float deltaX, float deltaY);
		void zoom(float zx, float zy, float length);
		void transform(WarGrey::DTPM::MapMove move);
		void transform(WarGrey::DTPM::MapMove move, float sx, float sy);
		
	private:
		void preshape(WarGrey::DTPM::IDigDatum* dig);
		void scale_transform(double zoom, float anchor_x, float anchor_y);
		
	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ plainfont;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> plaintexts;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> numtexts;
		std::map<WarGrey::DTPM::FontTextDig*, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> fonttexts;

	private:
		WarGrey::DTPM::DigDoc^ map;
		double geo_x;
		double geo_y;
		double geo_width;
		double geo_height;
		float width;
		float height;

	private:
		float xtranslation;
		float ytranslation;
		float tdelta;
		float zdelta;
		double ztimes;
		double fstimes;
		double _scale;
	};
}
