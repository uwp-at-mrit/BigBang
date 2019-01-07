#include <map>

#include "widget/history.hpp"
#include "configuration.hpp"

#include "page/hydraulics.hpp"

#include "system.hpp"
#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

/*************************************************************************************************/
private class TimeMachine : public ITimeMachine {
public:
	TimeMachine() : ITimeMachine(make_system_logger(default_logging_level, __MODULE__)) {}

	void fill_timemachine_extent(float* width, float* height) {
		float margin = normal_font_size * 2.0F;
		Size size = system_screen_size();

		SET_BOX(width, size.Width - margin);
		SET_BOX(height, size.Height - margin);
	}

public:
	void construct() override {
		this->push_planet(new HydraulicsPage());
	}
};

/*************************************************************************************************/
static ITimeMachine* the_timemachine = nullptr;

void WarGrey::SCADA::launch_the_timemachine() {
	if (the_timemachine == nullptr) {
		the_timemachine = new TimeMachine();
	}

	the_timemachine->show();
}
