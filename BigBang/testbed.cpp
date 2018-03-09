#include "testbed.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "turtle.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

Testbed::Testbed() : Planet("Test Bench") {
}

void Testbed::load(CanvasCreateResourcesReason reason, float width, float height) {
}

void Testbed::reflow(float width, float height) {
}
