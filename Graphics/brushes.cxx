#include "brushes.hxx"
#include "paint.hpp"
#include "colorspace.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas::Brushes;

CanvasSolidColorBrush^ Colours::make(double hue, double saturation, double lightness, double alpha) {
	return make_solid_brush(hsla(hue, saturation, lightness, alpha));
}

CanvasSolidColorBrush^ Colours::make(unsigned int hex, double alpha) {
	return make_solid_brush(hex, alpha);
}

CanvasSolidColorBrush^ Colours::contrast(CanvasSolidColorBrush^ src) {
	return make_solid_brush(contrast_color(src->Color));
}

CanvasSolidColorBrush^ Colours::Background::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(system_color(UIElementType::Background));

	return brush;
}

CanvasSolidColorBrush^ Colours::Foreground::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(system_color(UIElementType::HighlightText));

	return brush;
}

CanvasSolidColorBrush^ Colours::Highlight::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(system_color(UIElementType::Highlight));

	return brush;
}

CanvasSolidColorBrush^ Colours::GrayText::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(system_color(UIElementType::GrayText));

	return brush;
}

CanvasSolidColorBrush^ Colours::AccentDark::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(system_color(UIColorType::AccentDark1));

	return brush;
}

CanvasSolidColorBrush^ Colours::Transparent::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(Colors::Transparent);

	return brush;
}

CanvasSolidColorBrush^ Colours::OrangeRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 69, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Tomato::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 99, 71));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 139, 0, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Red::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 0, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Firebrick::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 178, 34, 34));

	return brush;
}

CanvasSolidColorBrush^ Colours::Crimson::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 220, 20, 60));

	return brush;
}

CanvasSolidColorBrush^ Colours::DeepPink::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 20, 147));

	return brush;
}

CanvasSolidColorBrush^ Colours::Maroon::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 176, 48, 96));

	return brush;
}

CanvasSolidColorBrush^ Colours::IndianRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 205, 92, 92));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumVioletRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 199, 21, 133));

	return brush;
}

CanvasSolidColorBrush^ Colours::VioletRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 208, 32, 144));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightCoral::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 240, 128, 128));

	return brush;
}

CanvasSolidColorBrush^ Colours::HotPink::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 105, 180));

	return brush;
}

CanvasSolidColorBrush^ Colours::PaleVioletRed::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 219, 112, 147));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightPink::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 182, 193));

	return brush;
}

CanvasSolidColorBrush^ Colours::RosyBrown::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 188, 143, 143));

	return brush;
}

CanvasSolidColorBrush^ Colours::Pink::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 192, 203));

	return brush;
}

CanvasSolidColorBrush^ Colours::Orchid::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 218, 112, 214));

	return brush;
}

CanvasSolidColorBrush^ Colours::LavenderBlush::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 240, 245));

	return brush;
}

CanvasSolidColorBrush^ Colours::Snow::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 250, 250));

	return brush;
}

CanvasSolidColorBrush^ Colours::Chocolate::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 210, 105, 30));

	return brush;
}

CanvasSolidColorBrush^ Colours::SaddleBrown::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 139, 69, 19));

	return brush;
}

CanvasSolidColorBrush^ Colours::Brown::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 132, 60, 36));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkOrange::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 140, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Coral::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 127, 80));

	return brush;
}

CanvasSolidColorBrush^ Colours::Sienna::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 160, 82, 45));

	return brush;
}

CanvasSolidColorBrush^ Colours::Orange::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 165, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Salmon::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 250, 128, 114));

	return brush;
}

CanvasSolidColorBrush^ Colours::Peru::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 205, 133, 63));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkGoldenrod::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 184, 134, 11));

	return brush;
}

CanvasSolidColorBrush^ Colours::Goldenrod::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 218, 165, 32));

	return brush;
}

CanvasSolidColorBrush^ Colours::SandyBrown::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 244, 164, 96));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightSalmon::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 160, 122));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkSalmon::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 233, 150, 122));

	return brush;
}

CanvasSolidColorBrush^ Colours::Gold::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 215, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Yellow::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 255, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Olive::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 128, 128, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Burlywood::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 222, 184, 135));

	return brush;
}

CanvasSolidColorBrush^ Colours::Tan::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 210, 180, 140));

	return brush;
}

CanvasSolidColorBrush^ Colours::NavajoWhite::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 222, 173));

	return brush;
}

CanvasSolidColorBrush^ Colours::PeachPuff::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 218, 185));

	return brush;
}

CanvasSolidColorBrush^ Colours::Khaki::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 240, 230, 140));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkKhaki::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 189, 183, 107));

	return brush;
}

CanvasSolidColorBrush^ Colours::Moccasin::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 228, 181));

	return brush;
}

CanvasSolidColorBrush^ Colours::Wheat::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 245, 222, 179));

	return brush;
}

CanvasSolidColorBrush^ Colours::Bisque::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 228, 196));

	return brush;
}

CanvasSolidColorBrush^ Colours::PaleGoldenrod::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 238, 232, 170));

	return brush;
}

CanvasSolidColorBrush^ Colours::BlanchedAlmond::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 235, 205));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumGoldenrod::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 234, 234, 173));

	return brush;
}

CanvasSolidColorBrush^ Colours::PapayaWhip::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 239, 213));

	return brush;
}

CanvasSolidColorBrush^ Colours::MistyRose::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 228, 225));

	return brush;
}

CanvasSolidColorBrush^ Colours::LemonChiffon::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 250, 205));

	return brush;
}

CanvasSolidColorBrush^ Colours::AntiqueWhite::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 250, 235, 215));

	return brush;
}

CanvasSolidColorBrush^ Colours::Cornsilk::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 248, 220));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightGoldenrodYellow::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 250, 250, 210));

	return brush;
}

CanvasSolidColorBrush^ Colours::OldLace::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 253, 245, 230));

	return brush;
}

CanvasSolidColorBrush^ Colours::Linen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 250, 240, 230));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightYellow::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 255, 224));

	return brush;
}

CanvasSolidColorBrush^ Colours::SeaShell::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 245, 238));

	return brush;
}

CanvasSolidColorBrush^ Colours::Beige::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 245, 245, 220));

	return brush;
}

CanvasSolidColorBrush^ Colours::FloralWhite::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 250, 240));

	return brush;
}

CanvasSolidColorBrush^ Colours::Ivory::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 255, 240));

	return brush;
}

CanvasSolidColorBrush^ Colours::Green::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 255, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::LawnGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 124, 252, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::Chartreuse::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 127, 255, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::GreenYellow::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 173, 255, 47));

	return brush;
}

CanvasSolidColorBrush^ Colours::YellowGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 154, 205, 50));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumForestGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 107, 142, 35));

	return brush;
}

CanvasSolidColorBrush^ Colours::OliveDrab::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 107, 142, 35));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkOliveGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 85, 107, 47));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkSeaGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 143, 188, 139));

	return brush;
}

CanvasSolidColorBrush^ Colours::Lime::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 255, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 100, 0));

	return brush;
}

CanvasSolidColorBrush^ Colours::LimeGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 50, 205, 50));

	return brush;
}

CanvasSolidColorBrush^ Colours::ForestGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 34, 139, 34));

	return brush;
}

CanvasSolidColorBrush^ Colours::SpringGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 255, 127));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumSpringGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 250, 154));

	return brush;
}

CanvasSolidColorBrush^ Colours::SeaGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 46, 139, 87));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumSeaGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 60, 179, 113));

	return brush;
}

CanvasSolidColorBrush^ Colours::Aquamarine::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 112, 216, 144));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 144, 238, 144));

	return brush;
}

CanvasSolidColorBrush^ Colours::PaleGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 152, 251, 152));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumAquamarine::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 102, 205, 170));

	return brush;
}

CanvasSolidColorBrush^ Colours::Turquoise::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 64, 224, 208));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightSeaGreen::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 32, 178, 170));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumTurquoise::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 72, 209, 204));

	return brush;
}

CanvasSolidColorBrush^ Colours::Honeydew::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 240, 255, 240));

	return brush;
}

CanvasSolidColorBrush^ Colours::MintCream::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 245, 255, 250));

	return brush;
}

CanvasSolidColorBrush^ Colours::RoyalBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 65, 105, 225));

	return brush;
}

CanvasSolidColorBrush^ Colours::DodgerBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 30, 144, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::DeepSkyBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 191, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::CornflowerBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 100, 149, 237));

	return brush;
}

CanvasSolidColorBrush^ Colours::SteelBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 70, 130, 180));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightSkyBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 135, 206, 250));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkTurquoise::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 206, 209));

	return brush;
}

CanvasSolidColorBrush^ Colours::Cyan::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 255, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::Aqua::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 255, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkCyan::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 139, 139));

	return brush;
}

CanvasSolidColorBrush^ Colours::Teal::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 128, 128));

	return brush;
}

CanvasSolidColorBrush^ Colours::SkyBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 135, 206, 235));

	return brush;
}

CanvasSolidColorBrush^ Colours::CadetBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 96, 160, 160));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkSlateGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 47, 79, 79));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightSlateGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 119, 136, 153));

	return brush;
}

CanvasSolidColorBrush^ Colours::SlateGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 112, 128, 144));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightSteelBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 176, 196, 222));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 173, 216, 230));

	return brush;
}

CanvasSolidColorBrush^ Colours::PowderBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 176, 224, 230));

	return brush;
}

CanvasSolidColorBrush^ Colours::PaleTurquoise::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 175, 238, 238));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightCyan::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 224, 255, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::AliceBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 240, 248, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::Azure::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 240, 255, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 0, 205));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 0, 139));

	return brush;
}

CanvasSolidColorBrush^ Colours::MidnightBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 25, 25, 112));

	return brush;
}

CanvasSolidColorBrush^ Colours::Navy::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 36, 36, 140));

	return brush;
}

CanvasSolidColorBrush^ Colours::Blue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 0, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::Indigo::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 75, 0, 130));

	return brush;
}

CanvasSolidColorBrush^ Colours::BlueViolet::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 138, 43, 226));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumSlateBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 123, 104, 238));

	return brush;
}

CanvasSolidColorBrush^ Colours::SlateBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 106, 90, 205));

	return brush;
}

CanvasSolidColorBrush^ Colours::Purple::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 160, 32, 240));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkSlateBlue::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 72, 61, 139));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkViolet::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 148, 0, 211));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkOrchid::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 153, 50, 204));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumPurple::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 147, 112, 219));

	return brush;
}

CanvasSolidColorBrush^ Colours::MediumOrchid::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 186, 85, 211));

	return brush;
}

CanvasSolidColorBrush^ Colours::Magenta::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 0, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::Fuchsia::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 0, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkMagenta::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 139, 0, 139));

	return brush;
}

CanvasSolidColorBrush^ Colours::Violet::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 238, 130, 238));

	return brush;
}

CanvasSolidColorBrush^ Colours::Plum::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 221, 160, 221));

	return brush;
}

CanvasSolidColorBrush^ Colours::Lavender::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 230, 230, 250));

	return brush;
}

CanvasSolidColorBrush^ Colours::Thistle::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 216, 191, 216));

	return brush;
}

CanvasSolidColorBrush^ Colours::GhostWhite::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 248, 248, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::White::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 255, 255, 255));

	return brush;
}

CanvasSolidColorBrush^ Colours::WhiteSmoke::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 245, 245, 245));

	return brush;
}

CanvasSolidColorBrush^ Colours::Gainsboro::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 220, 220, 220));

	return brush;
}

CanvasSolidColorBrush^ Colours::LightGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 211, 211, 211));

	return brush;
}

CanvasSolidColorBrush^ Colours::Silver::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 192, 192, 192));

	return brush;
}

CanvasSolidColorBrush^ Colours::Gray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 190, 190, 190));

	return brush;
}

CanvasSolidColorBrush^ Colours::DarkGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 169, 169, 169));

	return brush;
}

CanvasSolidColorBrush^ Colours::DimGray::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 105, 105, 105));

	return brush;
}

CanvasSolidColorBrush^ Colours::Black::get() {
	static CanvasSolidColorBrush^ brush = make_solid_brush(ColorHelper::FromArgb(255, 0, 0, 0));

	return brush;
}
