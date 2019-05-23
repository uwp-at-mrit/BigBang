#include "graphlet/planetlet.hpp"
#include "virtualization/screen/frame.hpp"

#include "planet.hpp"
#include "shape.hpp"
#include "transformation.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Devices::Input;
using namespace Windows::UI::Input;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

using namespace Windows::Foundation::Numerics;

namespace {
	private class PlaceholderPlanet : public Planet {
	public:
		virtual ~PlaceholderPlanet() noexcept {}
		PlaceholderPlanet() : Planet("_") {}
	};

	private class PlanetInfo : public WarGrey::SCADA::IPlanetInfo {
	public:
		PlanetInfo(IScreen* master) : IPlanetInfo(master) {};
	};
}

static inline PlanetInfo* bind_subplanet_owership(IScreen* master, IPlanet* planet) {
	auto info = new PlanetInfo(master);

	planet->info = info;

	return info;
}

static void construct_subplanet(IPlanet* planet, Platform::String^ type, Syslog* logger, CanvasCreateResourcesReason reason, float width, float height) {
	planet->begin_update_sequence();

	try {
		planet->construct(reason, width, height);
		planet->load(reason, width, height);
		planet->reflow(width, height);
		planet->notify_surface_ready();

		logger->log_message(Log::Debug, L"%s[%s] is constructed in region[%f, %f]", type->Data(), planet->name()->Data(), width, height);
	} catch (Platform::Exception ^ e) {
		logger->log_message(Log::Critical, L"%s: constructing: %s", planet->name()->Data(), e->Message->Data());
	}

	planet->end_update_sequence();
}

/**************************************************************************************************/
Planetlet::Planetlet(IPlanet* planet, float width, float height, ICanvasBrush^ background)
	: planet(planet), width(width), height(height), stretchable_width(false), stretchable_height(false), stretching_anchor(GraphletAnchor::CC) {
	if (this->planet == nullptr) {
		this->planet = new PlaceholderPlanet();
	}

	if (background != nullptr) {
		this->planet->set_background(background);
	}

	this->screen = new Frame(this);
	this->enable_events(true, true);
}

Planetlet::Planetlet(IPlanet* planet, GraphletAnchor anchor, ICanvasBrush^ background)
	: Planetlet(planet, 0.0F, 0.0F, background) {
	this->stretching_anchor = anchor;
}

Planetlet::~Planetlet() {
	delete this->planet;
	delete this->screen;
}

void Planetlet::construct() {
	if (this->width <= 0.0F) {
		this->stretchable_width = true;
		this->width = 0.0F;
	}

	if (this->height <= 0.0F) {
		this->stretchable_height = true;
		this->height = 0.0F;
	}

	bind_subplanet_owership(this->screen, this->planet);
	construct_subplanet(this->planet, "subplanet", this->get_logger(), CanvasCreateResourcesReason::FirstTime, this->width, this->height);
}

void Planetlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Planetlet::update(long long count, long long interval, long long uptime) {
	this->planet->begin_update_sequence();
	this->planet->on_elapse(count, interval, uptime);
	this->planet->end_update_sequence();

	if (this->stretchable_width || this->stretchable_height) {
		// all null arguments, just for triggling the resizing event
		this->moor(this->stretching_anchor);
		this->planet->fill_graphlets_boundary(nullptr, nullptr, nullptr, nullptr);
	}
}

void Planetlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float3x2 saved_transform = ds->Transform;

	ds->Transform = make_translation_matrix(x, y);
	this->planet->draw(ds, Width, Height);

	ds->Transform = saved_transform;
}

bool Planetlet::resize(float width, float height) {
	bool resized = false;

	if (this->stretchable_width) {
		if ((this->width != width) && (width > 0.0F)) {
			this->width = width;
			resized |= true;
		}
	}

	if (this->stretchable_height) {
		if ((this->height != height) && (height > 0.0F)) {
			this->height = height;
			resized |= true;
		}
	}

	return resized;
}

void Planetlet::enable_resize(bool resizable_width, bool resizable_height) {
	this->stretchable_width = resizable_width;
	this->stretchable_height = resizable_height;
}

void Planetlet::enable_resize(bool yes_or_no) {
	this->enable_resize(yes_or_no, yes_or_no);
}

void Planetlet::set_stretch_anchor(GraphletAnchor anchor) {
	this->stretching_anchor = anchor;
}

bool Planetlet::on_pointer_moved(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	return this->planet->on_pointer_moved(x, y, type, puk);
}

bool Planetlet::on_pointer_pressed(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	return this->planet->on_pointer_pressed(x, y, type, puk);
}

bool Planetlet::on_pointer_released(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	return this->planet->on_pointer_released(x, y, type, puk);
}

bool Planetlet::on_pointer_moveout(float x, float y, PointerDeviceType type, PointerUpdateKind puk) {
	return this->planet->on_pointer_moveout(x, y, type, puk);
}

bool Planetlet::on_key(VirtualKey key, bool screen_keyboard) {
	return this->planet->on_key(key, screen_keyboard);
}

bool Planetlet::on_character(unsigned int keycode) {
	return this->planet->on_character(keycode);
}
