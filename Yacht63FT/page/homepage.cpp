#include "page/homepage.hpp"
#include "configuration.hpp"

#include "system.hpp"
#include "text.hpp"
#include "math.hpp"

#include "graphlet/bitmaplet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class DefaultPage final : public WarGrey::SCADA::MRConfirmation {
public:
	DefaultPage(Homepage* master) : master(master) {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void load_and_flow(float width, float height) {
		this->yacht = new Bitmaplet("1_74.png", 0.0F, height);

		this->master->insert(this->yacht, width * 0.5F, height * 0.5F, GraphletAlignment::CC);
	}

// never deletes these graphlets mannually
private:
	Bitmaplet* yacht;
	Bitmaplet* copyright;
		
private:
	Homepage* master;
	CanvasTextFormat^ caption_font;
};

Homepage::Homepage() : Planet(":homepage:") {
	DefaultPage* console = new DefaultPage(this);

	this->console = console;
}

Homepage::~Homepage() {
	if (this->console != nullptr) {
		delete this->console;
	}
}

void Homepage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto console = dynamic_cast<DefaultPage*>(this->console);
	
	if (console != nullptr) {
		console->load_and_flow(width, height);
	}
}
