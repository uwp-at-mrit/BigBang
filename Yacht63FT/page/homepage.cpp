﻿#include "page/homepage.hpp"
#include "configuration.hpp"

#include "tongue.hpp"
#include "system.hpp"
#include "syslog.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"

#include "graphlet/bitmaplet.hpp"

#ifdef _DEBUG
#include "decorator/grid.hpp"
#endif

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HSMode { WindowUI = 0, View };

private class DefaultPage final : public WarGrey::SCADA::MRConfirmation {
public:
	DefaultPage(Homepage* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void load(float width, float height, float gridsize) {
		this->yacht = new Bitmaplet("1_74.png", 64.0F, 64.0F);
		this->master->insert(this->yacht);
	}

	
	void reflow(float width, float height, float gridsize, float vinset) {
	}

// never deletes these graphlets mannually
private:
	Bitmaplet* yacht;
	Bitmaplet* copyright;
		
private:
	Homepage* master;
	CanvasTextFormat^ caption_font;
};

Homepage::Homepage(IMRMaster* plc) : Planet(":homepage:"), device(plc) {
	DefaultPage* console = new DefaultPage(this);

	this->console = console; 
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(console);
}

Homepage::~Homepage() {
	if (this->console != nullptr) {
		delete this->console;
	}
}

void Homepage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<DefaultPage*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HSMode::View);
			console->load(width, height, this->gridsize);

			this->change_mode(HSMode::WindowUI);
		}

		{ // delayed initializing
#ifdef _DEBUG
			this->set_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, 0.0F));
#endif
		}
	}
}

void Homepage::reflow(float width, float height) {
	auto console = dynamic_cast<DefaultPage*>(this->console);
	
	if (console != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HSMode::WindowUI);

		this->change_mode(HSMode::View);
		console->reflow(width, height, this->gridsize, vinset);
	}
}

void Homepage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	this->set_selected(g);
	// this->set_caret_owner(g);
}
