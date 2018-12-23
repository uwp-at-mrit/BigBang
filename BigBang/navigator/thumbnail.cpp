#include <map>

#include "navigator/thumbnail.hpp"
#include "decorator/decorator.hpp"

#include "universe.hxx"
#include "planet.hpp"
#include "box.hpp"

#include "graphlet/textlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Input;

using namespace Windows::Devices::Input;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ caption_font = make_bold_text_format("Microsoft YaHei", 20.0F);
static CanvasTextFormat^ label_font = make_bold_text_format("Consolas", 14.0F);

/*************************************************************************************************/
private class Navigationlet : public IGraphlet {
public:
	Navigationlet(unsigned int id, IPlanet* entity, float width, float height) : entity(entity), id(id), width(width), height(height) {}

public:
	void construct() override {
		this->label = make_text_layout(entity->display_name(), label_font);
		this->mask_color = Colours::make(0x000000, 0.64);
	}

	void fill_extent(float x, float y, float* w, float* h) override {
		SET_VALUES(w, this->width, h, this->height);
	};

	void draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		Rect box = this->label->LayoutBounds;
		float cx = x + this->width * 0.5F;
		float ly = y + this->height - box.Height;

		if (this->thumbnail == nullptr) {
			this->thumbnail = entity->take_snapshot(entity->actual_width(), entity->actual_height(), Colours::Transparent);
		}

		ds->DrawImage(this->thumbnail, Rect(x, y, this->width, this->height));

		ds->FillRectangle(x, ly, this->width, box.Height, this->mask_color);
		ds->DrawTextLayout(this->label, cx - box.Width * 0.5F - box.X, ly, Colours::White);
	};

public:
	int index() {
		return this->id;
	}

private:
	CanvasRenderTarget^ thumbnail;
	CanvasTextLayout^ label;
	ICanvasBrush^ mask_color;

private:
	float width;
	float height;

private:
	IPlanet* entity; // managed by its original `UniverseDisplay`.
	unsigned int id;
};

private class NavigationPlanet : public Planet {
public:
	NavigationPlanet(IUniverseNavigator* navigator, Platform::String^ title) : Planet(title), master(navigator) {}

public:
	bool can_select(IGraphlet* g) override {
		return true;
	}

	bool on_pointer_pressed(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
		return true;
	}

	bool on_pointer_moved(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
		return true;
	}

	bool on_pointer_released(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
		switch (puk) {
		case PointerUpdateKind::LeftButtonReleased:
		case PointerUpdateKind::LeftButtonPressed: {
			auto this_graphlet = dynamic_cast<Navigationlet*>(this->find_graphlet(x, y));
			auto last_graphlet = dynamic_cast<Navigationlet*>(this->find_next_selected_graphlet());

			if ((this_graphlet != nullptr) && (last_graphlet != nullptr) && (this_graphlet != last_graphlet)) {
				this->master->navigate(last_graphlet->index(), this_graphlet->index());
				this->last_thumbnail = this_graphlet;
			}
		}; break;
		}

		return true;
	}

private:
	IUniverseNavigator* master;
	Navigationlet* last_thumbnail;
};


private class NavigationDecorator : public IPlanetDecorator {
public:
	NavigationDecorator(float vinset) : vinset(vinset) {}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		Rect box = this->caption->LayoutBounds;

		ds->DrawTextLayout(this->caption, (Width - box.Width) * 0.5F, vinset, Colours::Snow);
	}

	void draw_after_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) override {
		if (!selected) {
			ds->DrawRectangle(x, y, width, height, Colours::DimGray, 2.0F);
		}
	}

public:
	void on_active_planet_changed(IPlanet* master) override {
		this->caption = make_text_layout(master->display_name(), caption_font);
	}

public:
	float reserved_height() {
		return this->vinset + this->caption->LayoutBounds.Height;
	}

private:
	CanvasTextLayout^ caption;
	float vinset;
};

private ref class WarGrey::SCADA::NavigationDisplay : public UniverseDisplay {
internal:
	NavigationDisplay(IUniverseNavigator* navigator, Log level, Platform::String^ title
		, float ratio, float cell_width, unsigned int column, float gapsize)
		: UniverseDisplay(make_system_logger(level, title), nullptr, nullptr, new NavigationPlanet(navigator, title))
		, column(column), cell_width(cell_width), cell_height(cell_width / ratio), gapsize(gapsize)
		, decorator(new NavigationDecorator(gapsize)) {

		this->min_width = (this->cell_width + this->gapsize) * float(this->column) + this->gapsize;
		dynamic_cast<Planet*>(this->current_planet)->append_decorator(this->decorator);
	}

internal:
	void insert(IPlanet* planet) {
		unsigned int idx = (unsigned int)(this->planets.size());
		Navigationlet* thumbnail = new Navigationlet(idx, planet, this->cell_width, this->cell_height);
		float x = (this->cell_width + this->gapsize) * float(idx % this->column) + this->gapsize;
		float y = (this->cell_height + this->gapsize) * float(idx / this->column) + this->gapsize;
		float yoff = this->decorator->reserved_height();

		this->current_planet->insert(thumbnail, x, y + yoff);
		this->planets.insert(std::pair<IPlanet*, Navigationlet*>(planet, thumbnail));
	}

	void select(IPlanet* planet) {
		auto it = this->planets.find(planet);

		if (it != this->planets.end()) {
			this->current_planet->set_selected(it->second);
		}
	}

	int selected_index() {
		auto selected = dynamic_cast<Navigationlet*>(this->current_planet->find_next_selected_graphlet(nullptr));
		int index = 0;

		if (selected != nullptr) {
			index = selected->index();
		}

		return index;
	}

private:
	unsigned int column;
	float cell_width;
	float cell_height;
	float gapsize;

private: // Never deletes these Navigationlets and decorator manually
	std::map<IPlanet*, Navigationlet*> planets;
	NavigationDecorator* decorator;
};

/*************************************************************************************************/
ThumbnailNavigator::ThumbnailNavigator(Log level, Platform::String^ title, float ratio, float cell_width, unsigned int column, float gapsize) {
	this->master = ref new NavigationDisplay(this, level, title, ratio, cell_width, column, gapsize);
}

void ThumbnailNavigator::insert(IPlanet* planet) {
	this->master->insert(planet);
}

void ThumbnailNavigator::select(IPlanet* planet) {
	this->master->select(planet);
}

int ThumbnailNavigator::selected_index() {
	return this->master->selected_index();
}

Control^ ThumbnailNavigator::user_interface() {
	return this->master->canvas;
}
