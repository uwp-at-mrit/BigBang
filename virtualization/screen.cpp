#include "virtualization/screen.hpp"

#include "datum/flonum.hpp"

#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;


static inline float display_contain_mode_scale(float to_width, float to_height, float from_width, float from_height) {
	return flmin(flmin(to_width / from_width, to_height / from_height), 1.0F);
}

/*************************************************************************************************/
IScreen::IScreen(Syslog* logger, DisplayFit mode, float target_width, float target_height, float source_width, float source_height)
	: logger((logger == nullptr) ? make_silent_logger("IScreen") : logger), mode(mode)
	, target_width(flmax(target_width, 0.0F)), target_height(flmax(target_height, 0.0F))
	, source_width(source_width), source_height(source_height) {
	this->logger->reference();
	
	if (this->source_width <= 0.0F) {
		this->source_width = this->target_width;
	}

	if (this->source_height <= 0.0F) {
		this->source_height = this->target_height;
	}

	if ((this->target_width * this->target_height) == 0.0F) {
		this->mode = DisplayFit::None;
	}
}

IScreen::~IScreen() {
	this->logger->destroy();
}

Syslog* IScreen::get_logger() {
	return this->logger;
}

void IScreen::apply_source_size(float src_width, float src_height) {
	this->view_width(this->sketch_to_application_width(src_width));
	this->view_height(this->sketch_to_application_height(src_height));
}

float IScreen::sketch_to_application_width(float sketch_width) {
	static Size screen = system_screen_size();
	float width = sketch_width;

	switch (this->mode) {
	case DisplayFit::Contain: {
		static float scale = display_contain_mode_scale(screen.Width, screen.Height, target_width, target_height);

		width = (sketch_width / this->source_width * this->target_width) * scale;
	}; break;
	case DisplayFit::Fill: {
		width = sketch_width * screen.Width / this->source_width;
	}; break;
	}

	return width;
}

float IScreen::sketch_to_application_height(float sketch_height) {
	static Size screen = system_screen_size();
	float height = sketch_height;

	switch (this->mode) {
	case DisplayFit::Contain: {
		static float scale = display_contain_mode_scale(screen.Width, screen.Height, target_width, target_height);

		height = (sketch_height / this->source_height * this->target_height) * scale;
	}; break;
	case DisplayFit::Fill: {
		height = sketch_height * screen.Height / this->source_height;
	}; break;
	}

	return height;
}
