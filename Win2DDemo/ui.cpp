#include "pch.h"
#include "ui.h"

using namespace Win2D::XAML;

StackPanel^ UI::MakeStackPanel(Panel^ parent, String^ id, Orientation direction) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Name = (id == nullptr) ? "" : id;

    UIElementCollection^ children = parent->Children;
    children->InsertAt(children->Size, child);

    return child;
};

ToggleSwitch^ UI::MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->Name = (id == nullptr) ? "" : id;
    child->OnContent = (onCaption == nullptr) ? child->Name : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    UIElementCollection^ children = parent->Children;
    children->InsertAt(children->Size, child);

    return child;
};

Canvas^ UI::MakeCanvas(Panel^ parent, String^ id) {
    auto child = ref new Canvas();

    child->Name = (id == nullptr) ? "" : id;

    UIElementCollection^ children = parent->Children;
    children->InsertAt(children->Size, child);

    return child;
};
