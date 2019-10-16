#pragma once

#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

namespace WarGrey::SCADA {
	private class Depthlet : public WarGrey::SCADA::IGraphlet {
	public:
		Depthlet(WarGrey::SCADA::XyzDoc^ depths, double width, double height);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ plainfont;
		std::map<Platform::String^, Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^> plaintexts;

	private:
		WarGrey::SCADA::XyzDoc^ depths;
		float width;
		float height;
	};
}
