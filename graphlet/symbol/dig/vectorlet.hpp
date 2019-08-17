#include "graphlet/matrix/diglet.hpp"

namespace WarGrey::SCADA {
	private class DigVectorlet : public WarGrey::SCADA::IGraphlet {
	public:
		DigVectorlet(WarGrey::SCADA::DigVectorMap^ map, double width, double height, double tx, double ty);

	public:
		void fill_extent(float x, float y, float* width, float* height);
		void resize(float width, float height);
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		WarGrey::SCADA::DigVectorMap^ map;
		float width;
		float height;
		double xscale;
		double yscale;
		double xtranslation;
		double ytranslation;
	};
}
