#include "navigator/listview.hpp"
#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

/*************************************************************************************************/
private ref class ListViewNavigatorListener {
internal:
	ListViewNavigatorListener(ListViewNavigator* master, ListView^ employee) : master(master), employee(employee) {
		this->employee->ItemClick += ref new ItemClickEventHandler(this, &ListViewNavigatorListener::do_transfer);
		this->employee->SelectionMode = ListViewSelectionMode::Single;
		this->employee->IsItemClickEnabled = true;
	}

public:
	void do_transfer(Platform::Object^ sender, ItemClickEventArgs^ args) {
		int from_idx = this->employee->SelectedIndex;
		
		this->employee->SelectedValue = args->ClickedItem;
		this->master->navigate(from_idx, this->employee->SelectedIndex);
	}

private:
	Windows::UI::Xaml::Controls::ListView^ employee;
	ListViewNavigator* master;
};

/*************************************************************************************************/
ListViewNavigator::ListViewNavigator(ListView^ master) {
	this->master = ((master == nullptr) ? ref new ListView() : master);
	this->listener = ref new ListViewNavigatorListener(this, this->master);
}

void ListViewNavigator::insert(IPlanet* planet) {
	this->master->Items->Append(planet->display_name());
}

void ListViewNavigator::select(IPlanet* planet) {
	this->master->SelectedValue = planet->display_name();
}

int ListViewNavigator::selected_index() {
	return this->master->SelectedIndex;
}

Control^ ListViewNavigator::user_interface() {
	return this->master;
}
