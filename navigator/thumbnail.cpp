#include <map>

#include "navigator/thumbnail.hpp"

#include "virtualization/screen/pasteboard.hpp"
#include "decorator/decorator.hpp"
#include "graphlet/ui/textlet.hpp"

#include "datum/box.hpp"

#include "universe.hxx"
#include "planet.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::GYDM;

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

static CanvasSolidColorBrush^ caption_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ label_color = Colours::Snow;
static CanvasSolidColorBrush^ border_color = Colours::DimGray;
static CanvasSolidColorBrush^ border_hicolor = Colours::RoyalBlue;

/*************************************************************************************************/
namespace {
	private class Navigationlet : public IGraphlet {
	public:
		Navigationlet(unsigned int id, IPlanet* entity, float width, float height)
			: entity(entity), master(nullptr), id(id), width(width), height(height) {}

	public:
		void construct() override {
			Platform::String^ label = entity->display_name();
			TextExtent te = get_text_extent(label, label_font);

			if (te.width <= this->width) {
				this->label = make_text_layout(label, label_font);
			} else {
				float origin_font_size = label_font->FontSize;

				label_font->FontSize = origin_font_size * (this->width / te.width);
				this->label = make_text_layout(label, label_font);
				label_font->FontSize = origin_font_size;
			}

			this->mask_color = Colours::make(0x000000, 0.72);
		}

		void fill_extent(float x, float y, float* w, float* h) override {
			SET_VALUES(w, this->width, h, this->height);
		};

		void draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
			Rect box = this->label->LayoutBounds;
			float cx = x + this->width * 0.5F;
			float mask_height = box.Height * 1.618F;
			float mask_y = y + this->height - mask_height;
			float label_y = y + this->height - box.Height;

			if (this->thumbnail == nullptr) {
				this->refresh();
			}

			if (this->thumbnail != nullptr) {
				ds->DrawImage(this->thumbnail, Rect(x, y, this->width, this->height));
			}

			ds->FillRectangle(x, mask_y, this->width, mask_height, this->mask_color);
			ds->DrawTextLayout(this->label, cx - box.Width * 0.5F - box.X, label_y, label_color);
		};

	public:
		int index() {
			return this->id;
		}

		void refresh() {
			if (this->master != this->entity->master()) {
				this->master = dynamic_cast<Pasteboard*>(this->entity->master());
			}

			if (this->master != nullptr) {
				Point pt = this->master->local_to_global_point(this->entity, 0.0F, 0.0F);
				IGraphlet* thumblet = this->entity->thumbnail_graphlet();

				if (thumblet == nullptr) {
					this->thumbnail = this->entity->take_snapshot(-pt.X, -pt.Y,
						this->master->view_width(), this->master->view_height(),
						Colours::Transparent);
				} else {
					this->thumbnail = thumblet->take_snapshot();
				}
			}
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
		Pasteboard* master;
		unsigned int id;
	};

	private class NavigationPlanet : public IHeadUpPlanet {
	public:
		NavigationPlanet(IUniverseNavigator* navigator, Platform::String^ title) : IHeadUpPlanet(title), master(navigator) {}

	public:
		bool can_select(IGraphlet* g) override {
			return true;
		}

		void draw_visible_selection(CanvasDrawingSession^ ds, float x, float y, float width, float height) override {
			// do nothing
			// The decorator does it.
		}

		bool on_pointer_pressed(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
			return true;
		}

		bool on_pointer_moved(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
			return true;
		}

		bool on_pointer_released(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) override {
			bool handled = false;

			switch (puk) {
			case PointerUpdateKind::LeftButtonReleased:
			case PointerUpdateKind::LeftButtonPressed: {
				auto this_graphlet = dynamic_cast<Navigationlet*>(this->find_graphlet(x, y));
				auto last_graphlet = dynamic_cast<Navigationlet*>(this->find_next_selected_graphlet());

				if ((this_graphlet != nullptr) && (last_graphlet != nullptr)) {
					if (this_graphlet != last_graphlet) {
						this->master->navigate(last_graphlet->index(), this_graphlet->index());
						this->last_thumbnail = this_graphlet;
					}

					this_graphlet->refresh();
					last_graphlet->refresh();

					handled = true;
				}
			}; break;
			}

			return handled;
		}

	private:
		IUniverseNavigator* master;
		Navigationlet* last_thumbnail;
	};

	private class NavigationDecorator : public IPlanetDecorator {
	public:
		NavigationDecorator(float vinset) : vinset(vinset) {}

	public:
		void draw_before(CanvasDrawingSession^ ds, float X, float Y, float Width, float Height) override {
			Rect box = this->caption->LayoutBounds;

			ds->DrawTextLayout(this->caption, (Width - box.Width) * 0.5F + X, vinset + Y, caption_color);
		}

		void draw_after_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) override {
			ds->DrawRectangle(x, y, width, height, (selected ? border_hicolor : border_color), 2.0F);
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
}

/*************************************************************************************************/
private ref class WarGrey::SCADA::NavigationDisplay : public UniverseDisplay {
internal:
	NavigationDisplay(IUniverseNavigator* navigator, Log level, Platform::String^ title
		, float ratio, float cell_width, unsigned int column, float gapsize)
		: UniverseDisplay(make_system_logger(level, title), nullptr, nullptr, new NavigationPlanet(navigator, title))
		, column(column), cell_width(cell_width), cell_height(cell_width / ratio), gapsize(gapsize)
		, decorator(new NavigationDecorator(gapsize)) {

		this->use_global_mask_setting(false);
		this->min_width = (this->cell_width + this->gapsize) * float(this->column) + this->gapsize;
		dynamic_cast<Planet*>(this->heads_up_planet)->push_decorator(this->decorator);
	}

internal:
	void insert(IPlanet* planet) {
		unsigned int idx = (unsigned int)(this->planets.size());
		Navigationlet* thumbnail = new Navigationlet(idx, planet, this->cell_width, this->cell_height);
		float x = (this->cell_width + this->gapsize) * float(idx % this->column) + this->gapsize;
		float y = (this->cell_height + this->gapsize) * float(idx / this->column) + this->gapsize;
		float yoff = this->decorator->reserved_height();

		this->heads_up_planet->insert(thumbnail, x, y + yoff);
		this->planets.insert(std::pair<IPlanet*, Navigationlet*>(planet, thumbnail));
	}

	void select(IPlanet* planet) {
		auto it = this->planets.find(planet);

		if (it != this->planets.end()) {
			this->heads_up_planet->set_selected(it->second);
		}
	}

	int selected_index() {
		auto selected = dynamic_cast<Navigationlet*>(this->heads_up_planet->find_next_selected_graphlet(nullptr));
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
