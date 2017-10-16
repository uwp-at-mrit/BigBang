#include "control.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Microsoft::Graphics::Canvas;

/*************************************************************************************************/
UserControl^ Win2DControl::canvas::get() { return this->control; };
CanvasDevice^ Win2DControl::device::get() { return this->universe->Device; };

float Win2DControl::actual_width::get() { return float(this->control->ActualWidth); };
float Win2DControl::actual_height::get() { return float(this->control->ActualHeight); };

void Win2DControl::max_width::set(float v) { this->control->MaxWidth = double(v); }
float Win2DControl::max_width::get() { return float(this->control->MaxWidth); };

void Win2DControl::max_height::set(float v) { this->control->MaxHeight = double(v); }
float Win2DControl::max_height::get() { return float(this->control->MaxHeight); };

void Win2DControl::min_width::set(float v) { this->control->MinWidth = double(v); }
float Win2DControl::min_width::get() { return float(this->control->MinWidth); };

void Win2DControl::min_height::set(float v) { this->control->MinHeight = double(v); }
float Win2DControl::min_height::get() { return float(this->control->MinHeight); };

void Win2DControl::width::set(float v) { this->control->Width = double(v); }
float Win2DControl::width::get() { return float(this->control->Width); };

void Win2DControl::height::set(float v) { this->control->Height = double(v); }
float Win2DControl::height::get() { return float(this->control->Height); };

void Win2DControl::fill_actual_extent(float* width, float* height) {
    if (width != nullptr)  (*width)  = this->actual_width;
    if (height != nullptr) (*height) = this->actual_height;
}
