#include "hamburger.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

static void configure_flyout(Flyout^ ui, IHamburger* self) {
	Style^ style = ref new Style(FlyoutPresenter::typeid); // WARNING: the style can only be modified before it is applied
	Thickness border, padding;
	double wspread, hspread;
	float width, height;

	self->fill_extent(&width, &height);
	self->fill_border(border);
	self->fill_padding(padding);

	wspread = (border.Left + border.Right + padding.Left + padding.Right);
	hspread = (border.Top + border.Bottom + padding.Top + padding.Bottom);

	style->Setters->Append(ref new Setter(FlyoutPresenter::BorderThicknessProperty, border));
	style->Setters->Append(ref new Setter(FlyoutPresenter::PaddingProperty, padding));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxWidthProperty, double(width) + wspread));
	style->Setters->Append(ref new Setter(FlyoutPresenter::MaxHeightProperty, double(height) + hspread));

	ui->FlyoutPresenterStyle = style; // apply the style
}

/*************************************************************************************************/
void IHamburger::fill_border(Thickness& border) {
	double thickness = 1.0;

	border.Top = thickness;
	border.Right = thickness;
	border.Bottom = thickness;
	border.Left = thickness;
}

void IHamburger::fill_padding(Thickness& padding) {
	double space = 0.0;

	padding.Top = space;
	padding.Right = space;
	padding.Bottom = space;
	padding.Left = space;
}

void IHamburger::show() {
	FrameworkElement^ frame = dynamic_cast<FrameworkElement^>(Window::Current->Content);

	if (this->user_interface()->FlyoutPresenterStyle == nullptr) {
		configure_flyout(this->user_interface(), this);
	}

	this->user_interface()->ShowAt(frame);
}

void IHamburger::hide() {
	this->user_interface()->Hide();
}
