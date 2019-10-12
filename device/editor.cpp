#include "device/editor.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Windows::UI::Xaml::Controls::Primitives;

using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ caption_font = make_bold_text_format("Microsoft Yahei", 16.0F);
static CanvasSolidColorBrush^ caption_color = Colours::Azure;

/*************************************************************************************************/
EditorPlanet::EditorPlanet(Platform::String^ caption, unsigned int initial_mode) : Planet(caption, initial_mode) {}

void EditorPlanet::load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) {
	float btn_height, cpt_height, inset, bg_height;

	this->caption = new Labellet(this->display_name(), caption_font, caption_color);
	this->apply = new Buttonlet(ButtonState::Disabled, "_Apply");
	
	this->caption->fill_extent(0.0F, 0.0F, nullptr, &cpt_height);
	this->apply->fill_extent(0.0F, 0.0F, nullptr, &btn_height);
	inset = btn_height * 0.618F;
	bg_height = height - btn_height - cpt_height - inset * 4.0F;

	this->background = this->insert_one(new RoundedRectanglet(width - inset * 2.0F, bg_height, 8.0F, Colours::Background));
	
	this->insert(this->caption);
	this->insert(this->apply);
	
	this->discard = this->insert_one(new Buttonlet(ButtonState::Ready, "_Discard"));
	this->reset = this->insert_one(new Buttonlet(ButtonState::Ready, "_Reset"));
}

void EditorPlanet::reflow(float width, float height) {
	float vinset, bg_height, cpt_height, btn_height;

	this->caption->fill_extent(0.0F, 0.0F, nullptr, &cpt_height);
	this->background->fill_extent(0.0F, 0.0F, nullptr, &bg_height);
	this->apply->fill_extent(0.0F, 0.0F, nullptr, &btn_height);

	vinset = (height - cpt_height - bg_height - btn_height) * 0.25F;
	this->move_to(this->caption, width * 0.5F, vinset, GraphletAnchor::CT);
	this->move_to(this->background, this->caption, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, vinset);
	this->move_to(this->discard, this->background, GraphletAnchor::RB, GraphletAnchor::RT, 0.0F, vinset);
	this->move_to(this->apply, this->discard, GraphletAnchor::LC, GraphletAnchor::RC, -vinset * 0.5F);
	this->move_to(this->reset, this->apply, GraphletAnchor::LC, GraphletAnchor::RC, -vinset * 1.5F);
}

bool EditorPlanet::can_select(WarGrey::SCADA::IGraphlet* g) {
	Buttonlet* b = dynamic_cast<Buttonlet*>(g);
	IEditorlet* t = dynamic_cast<IEditorlet*>(g);

	return ((b != nullptr) && (b->get_state() != ButtonState::Disabled))
		|| ((t != nullptr) && (t->get_state() == DimensionState::Input));
}

bool EditorPlanet::on_key(VirtualKey key, bool wargrey_keyboard) {
	bool handled = Planet::on_key(key, wargrey_keyboard);

	if (!handled) {
		switch (key) {
		case VirtualKey::Enter: {
			auto editor = dynamic_cast<Dimensionlet*>(this->get_focus_graphlet());

			if (editor != nullptr) {
				this->hide_virtual_keyboard();
				this->set_caret_owner(nullptr);

				if (this->on_edit(editor)) {
					this->notify_modification();
				}

				handled = true;
			}
		}; break;
		}
	}

	return handled;
}

void EditorPlanet::on_focus(IGraphlet* g, bool yes) {
	if (yes) {
		auto editor = dynamic_cast<IEditorlet*>(g);

		if (editor != nullptr) {
			this->show_virtual_keyboard(ScreenKeyboard::Numpad);
		}
	}
}

void EditorPlanet::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	if (this->apply == g) {
		this->on_apply();
		this->apply->set_state(ButtonState::Disabled);
	} else if (this->reset == g) {
		this->on_reset();
	} else if (this->discard == g) {
		if (this->on_discard()) {
			auto flyout = FlyoutBase::GetAttachedFlyout(g->info->master->master()->display()->canvas);

			if (this->apply->get_state() != ButtonState::Disabled) {
				this->on_reset();
				this->apply->set_state(ButtonState::Disabled);
			}

			if (flyout != nullptr) {
				flyout->Hide();
			}
		}
	}
}

void EditorPlanet::notify_modification() {
	this->apply->set_state(ButtonState::Ready);
}
