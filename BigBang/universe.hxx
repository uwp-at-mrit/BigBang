#pragma once

#include <mutex>

#include "sugar.hpp"
#include "forward.hpp"
#include "syslog.hpp"

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

    private ref class IDisplay abstract {
	public:
		void enter_critical_section();
		void leave_critical_section();

	public:
		vpure_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		vpure_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);

    public:
        virtual_read_only_property(float, actual_width);
        virtual_read_only_property(float, actual_height);

	public:
        read_write_property(float, width);
        read_write_property(float, height);
        read_write_property(float, min_width);
        read_write_property(float, min_height);
        read_write_property(float, max_width);
        read_write_property(float, max_height);

	private:
		std::mutex section;
    };

	private ref class UniverseDisplay : public IDisplay {
	public:
		virtual ~UniverseDisplay();

	internal:
		UniverseDisplay(Platform::String^ name, int frame_rate, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug);

	public:
		read_only_property(Windows::UI::Xaml::UIElement^, navigator);
		override_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);
		override_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		override_read_only_property(float, actual_width);
		override_read_only_property(float, actual_height);
		
	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);
		void transfer(int delta_idx, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_previous(unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_next(unsigned int timeline_ms = 0, unsigned int frame_count = 4);

	public:
		virtual void big_bang() {};  // occurs at game loop thread
		virtual void construct() {}; // occurs at UI thread
		virtual void big_rip() {};   // occurs at game loop thread

	protected private:
		void add_planet(IPlanet* planet);
		void collapse();
		
	private:
		void do_refresh(Platform::Object^ sender, Platform::Object^ args);
		void do_transfer(Platform::Object^ sender, Windows::UI::Xaml::Controls::ItemClickEventArgs^ args);
		void do_resize(Platform::Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ args);
		void do_start(Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^ sender, Platform::Object^ args);
		void do_stop(Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^ sender, Platform::Object^ args);
		
		void do_construct(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedControl^ sender,
			Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);

		void do_update(
			Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^ sender,
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedUpdateEventArgs^ args);

		void do_paint(
			Microsoft::Graphics::Canvas::UI::Xaml::ICanvasAnimatedControl^ sender,
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedDrawEventArgs^ args);

	private:
		Windows::UI::Input::PointerPoint^ saved_pressed_pt;
		void on_pointer_pressed(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_released(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_maniplated(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationCompletedRoutedEventArgs^ args);

	private:
		Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedControl^ display;
		Windows::UI::Xaml::Controls::ListView^ navigator_view;
		WarGrey::SCADA::Syslog* logger;
		WarGrey::SCADA::IPlanet* head_planet;
		WarGrey::SCADA::IPlanet* current_planet;

	private:
		Windows::UI::Xaml::DispatcherTimer^ transfer_clock;
		WarGrey::SCADA::IPlanet* from_planet;
		float transfer_delta;
		float transferX;
	};
}
