#include "numpad.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

NumpadFlyout::NumpadFlyout() : Flyout() {
	this->numboard = ref new GridView();

	this->numboard->SelectionMode = ListViewSelectionMode::None;
	this->numboard->IsItemClickEnabled = true;
	
	//auto layout = dynamic_cast<ItemsWrapGrid^>(this->numboard->ItemsPanel);
	//layout->MaximumRowsOrColumns = 3;

	this->numboard->Header = "Numpad";
	for (unsigned int i = 0; i < 10; i++) {
		this->numboard->Items->Append(i.ToString());
	}

	this->Content = this->numboard;
}
