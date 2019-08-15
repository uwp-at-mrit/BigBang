#include "graphlet/matrix/diglet.hpp"

namespace WarGrey::SCADA {
	private class DigVectorlet : public WarGrey::SCADA::IGraphlet {
	public:
		DigVectorlet(WarGrey::SCADA::DigVectorMap^ map);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height);
		void resize(float width, float height);
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		WarGrey::SCADA::DigVectorMap^ map;
		double xscale;
		double yscale;
		double x;
		double y;
		double width;
		double height;
	};
}
