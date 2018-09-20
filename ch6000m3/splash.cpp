#include "splash.hpp"

#include "graphlet/shapelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;

private class Screen final {
public:
	Screen(SplashScreen* master) : master(master) {}

public:
	void load(float width, float height, float logo_width, float logo_height) {
		float border_size = 2.0F;
		float w = logo_width - border_size;
		float h = logo_height - border_size;

		this->frame = this->master->insert_one(new Rectanglet(w, h, Colours::Background, Colours::RoyalBlue, border_size));
	}

	void reflow(float width, float height, float logo_width, float logo_height) {
		this->master->move_to(this->frame, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
	}

private: // never delete these graphlets manually.
	Rectanglet* frame;
	
private:
	SplashScreen* master;
};

/*************************************************************************************************/
static std::unordered_map<SplashScreen*, Screen*> screens;

SplashScreen::SplashScreen(float square_size) : SplashScreen(square_size, square_size) {}

SplashScreen::SplashScreen(float wide_width, float wide_height)
	: Planet("icon"), logo_width(wide_width), logo_height(wide_height) {

	if (this->logo_height <= 0.0F) {
		this->logo_height = this->logo_width * 600.0F / 1240.0F;
	}
}

SplashScreen::~SplashScreen() {
	auto maybe_stage = screens.find(this);

	if (maybe_stage != screens.end()) {
		delete maybe_stage->second;

		screens.erase(maybe_stage);
	}
}

void SplashScreen::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (screens.find(this) == screens.end()) {
		Screen* stage = new Screen(this);
		
		screens.insert(std::pair<SplashScreen*, Screen*>(this, stage));

		stage->load(width, height, this->logo_width, this->logo_height);
	}
}

void SplashScreen::reflow(float width, float height) {
	auto maybe_stage = screens.find(this);
	
	if (maybe_stage != screens.end()) {
		Screen* stage = maybe_stage->second;
		
		stage->reflow(width, height, this->logo_width, this->logo_height);
	}
}
