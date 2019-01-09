#pragma once

namespace WarGrey::SCADA {
	private class IHamburger abstract {
	public:
		virtual void fill_extent(float* width, float* height) = 0;
		virtual void fill_border(Windows::UI::Xaml::Thickness& border);
		virtual void fill_padding(Windows::UI::Xaml::Thickness& padding);

	public:
		virtual void on_showing() {}
		virtual void on_shown() {}
		virtual bool can_hide() { return true; }
		virtual void on_hiden() {}

	public:
		void show();
		void hide();

	protected:
		virtual Windows::UI::Xaml::Controls::Flyout^ user_interface() = 0;
	};
}
