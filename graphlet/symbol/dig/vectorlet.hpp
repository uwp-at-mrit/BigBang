#include "graphlet/matrix/diglet.hpp"

namespace WarGrey::SCADA {
	private class DigVectorlet : public WarGrey::SCADA::IGraphlet {
	public:
		DigVectorlet(WarGrey::SCADA::DigVectorMap^ map, double width, double height, double tx, double ty);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width, float* height) override;
		void resize(float width, float height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		WarGrey::SCADA::DigVectorMap^ map;
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
