#include "test/cyberspace.hpp"

#include "decorator/border.hpp"

#include "graphlet/textlet.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;

/*************************************************************************************************/
CyberSpace::CyberSpace() : Planet("Cyber Space") {
	this->append_decorator(new BorderDecorator());
}

CyberSpace::~CyberSpace() {}

void CyberSpace::on_tap(IGraphlet* g, float x, float y) {
	if (g == nullptr) {
		this->insert(new Labellet(make_wstring(L"(%f, %f)", x, y)), x, y);
	}
}

void CyberSpace::on_right_tap(IGraphlet* g, float x, float y) {
	if (g != nullptr) {
		this->remove(g);
	}
}
