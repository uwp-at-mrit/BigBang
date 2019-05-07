#include "test/cyberspace.hpp"

#include "decorator/border.hpp"
#include "graphlet/textlet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Devices::Input;
using namespace Windows::UI::Input;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;

/*************************************************************************************************/
CyberSpace::CyberSpace() : Planet("Cyber Space"), escaped(nullptr) {
	this->push_decorator(new BorderDecorator());
}

CyberSpace::~CyberSpace() {}

void CyberSpace::on_tap(IGraphlet* g, float x, float y) {
	if (g == nullptr) {
		this->insert(new Labellet(make_wstring(L"(%f, %f)", x, y)), x, y);
	} else {
		this->remove(g);
	}
}

bool CyberSpace::on_key(VirtualKey key, bool screen_keyboard) {
	Platform::String^ label = key.ToString();
	float Width = this->actual_width();
	float Height = this->actual_height();
	float width, height;

	if (this->keycode != nullptr) {
		this->keycode->set_text(label);
	} else {
		this->keycode = this->insert_one(new Labellet(label));
	}

	this->keycode->fill_extent(0.0F, 0.0F, &width, &height);
	this->move_to(this->keycode, Width - width, Height - height);

	return Planet::on_key(key, screen_keyboard);
}

bool CyberSpace::on_pointer_moved(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	Platform::String^ label = make_wstring(L"(%f, %f)", x, y);
	
	if (this->hovered != nullptr) {
		this->hovered->set_text(label);
	} else {
		this->hovered = this->insert_one(new Labellet(label));
	}

	this->move_to(this->hovered, x, y);

	return Planet::on_pointer_moved(x, y, type, puk);
}

bool CyberSpace::on_pointer_moveout(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	if (this->hovered != nullptr) {
		this->remove(this->hovered);
		this->hovered = nullptr;
	}

	if (this->escaped != nullptr) {
		this->remove(this->escaped);
		this->escaped = nullptr;
	}

	{ // display escaped position
		float Width = this->actual_width();
		float Height = this->actual_height();
		float width, height;

		this->escaped = new Labellet(make_wstring(L"esacped: (%f, %f)", x, y));
		this->escaped->fill_extent(0.0F, 0.0F, &width, &height);

		this->insert(this->escaped, flmin(flmax(x, 0.0F), Width - width), flmin(flmax(y, 0.0F), Height - height));
	}

	return Planet::on_pointer_moveout(x, y, type, puk);
}
