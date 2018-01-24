#pragma once

#include <shared_mutex>

#include "universe.hxx"
#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	private class IPlanetInfo abstract {
	public:
		virtual ~IPlanetInfo() noexcept {};
		IPlanetInfo(IDisplay^ master) : master(master) {};
		
	public:
		IDisplay^ master;
	};

    private class IPlanet abstract {
    public:
		virtual ~IPlanet() noexcept {
			if (this->info != nullptr) {
				delete this->info;
				this->info = nullptr;
			}
		};
		
    public:
        virtual void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {};
		virtual void update(long long count, long long interval, long long uptime, bool is_slow) {}; 
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float Width, float Height) {};
		virtual void collapse() {};

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float width, float height, float dpi = 96.0);
		void save(Platform::String^ path, float width, float height, float dpi = 96.0);

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
		virtual ISnip* find_snip(float x, float y) = 0;
		virtual void fill_snip_location(ISnip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;
		virtual void fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_snips_bounds(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(ISnip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) = 0;
		virtual void move(ISnip* snip, float x, float y) = 0;
		virtual void move_to(ISnip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;

	public:
		virtual void add_selected(ISnip* snip) = 0;
		virtual void set_selected(ISnip* snip) = 0;
		virtual void no_selected() = 0;

    public:
		void enter_critical_section();
		void enter_shared_section();
		void leave_critical_section();
		void leave_shared_section();
		void fill_actual_extent(float* width, float* height);

	public:
		IPlanetInfo* info;

	private:
		std::shared_mutex section;
    };

	private class Planet : public WarGrey::SCADA::IPlanet {
	public:
		~Planet() noexcept;
		Planet();

    public:
        void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;
		void collapse() override;

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
        ISnip* find_snip(float x, float y) override;
        void fill_snip_location(ISnip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;
		void fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) override;
		void insert(ISnip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) override;
        void move(ISnip* snip, float x, float y) override;
        void move_to(ISnip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;

    public:
        void add_selected(ISnip* snip) override;
        void set_selected(ISnip* snip) override;
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
        WarGrey::SCADA::ISnip* head_snip = nullptr;
    };
}
