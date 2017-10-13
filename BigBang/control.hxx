#pragma once

#include "sugar.hpp"

namespace WarGrey::SCADA {
    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^,
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedDrawEventArgs^>
        UniverseDrawHandler;

    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedControl^,
        Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^>
        UniverseLoadHandler;

    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^,
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedUpdateEventArgs^>
        UniverseUpdateHandler;

    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^,
        Platform::Object^>
        UniverseHandler;

    private ref class Win2DControl abstract {
    public:
        read_only_property(float, actual_width);
        read_only_property(float, actual_height);

        read_write_property(float, width);
        read_write_property(float, height);
        read_write_property(float, min_width);
        read_write_property(float, min_height);
        read_write_property(float, max_width);
        read_write_property(float, max_height);

    public:
        read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
        read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);

    protected private:
        Microsoft::Graphics::Canvas::ICanvasResourceCreator^ universe;
        Windows::UI::Xaml::Controls::UserControl^ control;
    };
}
