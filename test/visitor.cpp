#include "test/visitor.hpp"
#include "test/cyberspace.hpp"

#include "decorator/border.hpp"
#include "graphlet/textlet.hpp"

#include "datum/string.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;

/*************************************************************************************************/
VisitorSpace::VisitorSpace() : Planet("Visitor Space") {
	this->push_decorator(new BorderDecorator());
}

VisitorSpace::~VisitorSpace() {}

void VisitorSpace::load(CanvasCreateResourcesReason reason, float width, float height) {

}

void VisitorSpace::on_tap(IGraphlet* g, float x, float y) {
	if (g == nullptr) {
		this->insert(new Labellet(make_wstring(L"(%f, %f)", x, y)), x, y);
	} else {
		this->remove(g);
	}
}
