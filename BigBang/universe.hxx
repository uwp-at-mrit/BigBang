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
		UniverseDisplay(int frame_rate, Platform::String^ name = "", WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug);

	public:
		override_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		override_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);
		override_read_only_property(float, actual_width);
		override_read_only_property(float, actual_height);
		
	public:
		void transfer(int delta_idx);
		void transfer_to(int idx);
		void transfer_previous();
		void transfer_next();

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);

	public:
		virtual void big_bang() {};  // occurs at game loop thread
		virtual void construct() {}; // occurs at UI thread
		virtual void big_rip() {};   // occurs at game loop thread

	protected private:
		void add_planet(IPlanet* planet);
		void collapse();
		
	private:
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
		void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_pressed(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_released(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);

	private:
		Microsoft::Graphics::Canvas::UI::Xaml::CanvasAnimatedControl^ display;
		WarGrey::SCADA::Syslog* logger;
		WarGrey::SCADA::IPlanet* head_planet;
		WarGrey::SCADA::IPlanet* current_planet;
	};
}
