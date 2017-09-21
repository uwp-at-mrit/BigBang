#pragma once

typedef Windows::Foundation::EventHandler<Platform::Object^> ObjectHandler;

typedef Windows::Foundation::TypedEventHandler<
    Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^,
    Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^>
    CanvasDrawHandler;

typedef Windows::Foundation::TypedEventHandler<
    Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^,
    Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^>
    CanvasLoadHandler;

Windows::UI::Xaml::Controls::StackPanel^ stack_panel(
    Windows::UI::Xaml::Controls::Panel^ parent,
    Windows::UI::Xaml::Controls::Orientation direction,
    Windows::UI::Xaml::Thickness margin,
    Windows::UI::Xaml::Thickness padding);

Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ gpu_canvas(
    Windows::UI::Xaml::Controls::Panel^ parent,
    Platform::String^ id,
    CanvasLoadHandler^ Load,
    CanvasDrawHandler^ draw);

Windows::UI::Xaml::DispatcherTimer^ gui_timer(
    long long ms,
    ObjectHandler^ handler);
