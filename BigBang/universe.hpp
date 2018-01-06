#pragma once

#include "control.hxx"
#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class IUniverse abstract {
    public:
        IUniverse(Windows::UI::Xaml::Controls::Panel^ parent, int frame_rate);
        virtual ~IUniverse() noexcept;

    public:
        virtual void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args, float Width, float Height) {};
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float Width, float Height) {};
		virtual void save(Platform::String^ path, float width, float height, float dpi = 96.0) = 0;
		virtual void clear() = 0;

    public:
        virtual void start() {};
        virtual void update(long long count, long long interval, long long uptime, bool is_slow) {};
        virtual void stop() {};

    public:
        virtual void reflow(float width, float height) {};

    public:
        virtual void on_pointer_moved(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args) {};

        virtual void on_pointer_pressed(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args) {};

        virtual void on_pointer_released(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args) {};

	public:
		virtual Snip * find_snip(float x, float y) = 0;
		virtual void fill_snip_location(Snip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;
		virtual void fill_snip_bound(Snip* snip, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_snips_bounds(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(Snip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) = 0;
		virtual void move(Snip* snip, float x, float y) = 0;
		virtual void move_to(Snip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;

	public:
		virtual void add_selected(Snip* snip) = 0;
		virtual void set_selected(Snip* snip) = 0;
		virtual void no_selected() = 0;

    public:
        void resize(float width, float height);
		void fill_actual_extent(float* width, float* height);

    protected:
        WarGrey::SCADA::Win2DControl^ control;
    };

    private class Universe : public WarGrey::SCADA::IUniverse {
    public:
        Universe(Windows::UI::Xaml::Controls::Panel^ parent, int frame_rate = 0);
        virtual ~Universe() noexcept;

    public:
        void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args, float Width, float Height) override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;
		void save(Platform::String^ path, float width, float height, float dpi = 96.0) override;
		void clear() override;

    public:
        virtual void on_pointer_moved(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args)
            override;

        virtual void on_pointer_pressed(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args)
            override;

        virtual void on_pointer_released(
            Windows::UI::Xaml::UIElement^ obj,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args)
            override;

    public:
        void fill_snips_bounds(float* x, float* y, float* width, float* height);
        void size_cache_invalid();

    public:
        void set_pointer_listener(WarGrey::SCADA::IUniverseListener* listener);
        void set_decorator(WarGrey::SCADA::IUniverseDecorator* decorator);

    public:
        Snip* find_snip(float x, float y) override;
        void fill_snip_location(Snip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;
		void fill_snip_bound(Snip* snip, float* x, float* y, float* width, float* height) override;
		void insert(Snip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) override;
        void move(Snip* snip, float x, float y) override;
        void move_to(Snip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;

    public:
        void add_selected(Snip* snip) override;
        void set_selected(Snip* snip) override;
        void no_selected() override;

    private:
        void recalculate_snips_extent_when_invalid();

    private:
        WarGrey::SCADA::IUniverseListener* listener;
        float last_pointer_x;
        float last_pointer_y;
        float rubberband_x[2];
        float* rubberband_y;
        bool rubberband_allowed;

    private:
        float snips_left;
        float snips_top;
        float snips_right;
        float snips_bottom;
        float preferred_min_width;
        float preferred_min_height;

    private:
        WarGrey::SCADA::IUniverseDecorator* decorator = nullptr;
        WarGrey::SCADA::Snip* head_snip = nullptr;
    };
}
