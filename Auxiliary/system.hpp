#pragma once

Windows::Foundation::Size adjusted_workspace_size(Windows::Foundation::Rect region, Windows::UI::Xaml::FrameworkElement^ ws);
Windows::Foundation::Size system_screen_size();

Windows::UI::Color system_color(Windows::UI::ViewManagement::UIColorType type);
Windows::UI::Color system_color(Windows::UI::ViewManagement::UIElementType type);
