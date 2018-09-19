#include "splash.hpp"

#include "graphlet/shapelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;

private class Canvas final {
public:
	Canvas(SplashScreen* master) : master(master) {}

public:
	void load(float width, float height, float logo_width, float logo_height) {
		float border_size = 2.0F;
		float w = logo_width - border_size;
		float h = logo_height - border_size;

		this->frame = this->master->insert_one(new Rectanglet(w, h, Colours::Snow, Colours::RoyalBlue, border_size));
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
static std::unordered_map<SplashScreen*, Canvas*> stages;

SplashScreen::SplashScreen(float square_size) : SplashScreen(square_size, square_size) {}

SplashScreen::SplashScreen(float wide_width, float wide_height)
	: Planet("icon"), logo_width(wide_width), logo_height(wide_height) {

	if (this->logo_height <= 0.0F) {
		this->logo_height = this->logo_width * 600.0F / 1240.0F;
	}
}

SplashScreen::~SplashScreen() {
	auto maybe_stage = stages.find(this);

	if (maybe_stage != stages.end()) {
		delete maybe_stage->second;

		stages.erase(maybe_stage);
	}
}

void SplashScreen::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (stages.find(this) == stages.end()) {
		Canvas* stage = new Canvas(this);
		
		stages.insert(std::pair<SplashScreen*, Canvas*>(this, stage));

		stage->load(width, height, this->logo_width, this->logo_height);
	}
}

void SplashScreen::reflow(float width, float height) {
	auto maybe_stage = stages.find(this);
	
	if (maybe_stage != stages.end()) {
		Canvas* stage = maybe_stage->second;
		
		stage->reflow(width, height, this->logo_width, this->logo_height);
	}
}
