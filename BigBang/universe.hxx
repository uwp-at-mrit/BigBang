#pragma once

#include <map>
#include <mutex>

#include "timer.hxx"

#include "class.hpp"
#include "forward.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^,
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^>
        UniverseDrawHandler;

    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^,
        Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^>
        UniverseLoadHandler;

    typedef Windows::Foundation::TypedEventHandler<
        Microsoft::Graphics::Canvas::UI::Xaml::ICanvasControl^,
        Platform::Object^>
        UniverseHandler;

	private enum class DisplayFit { Fill, Contain, None };

	private ref class IDisplay abstract : public WarGrey::SCADA::ITimerAction {
	public:
		virtual ~IDisplay();

	internal:
		IDisplay(WarGrey::SCADA::Syslog* logger,
			WarGrey::SCADA::DisplayFit mode = DisplayFit::None,
			float target_width = 0.0F, float target_height = 0.0F,
			float source_width = 0.0F, float source_height = 0.0F);

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

	public:
		void apply_source_size(float sketch_width, float sketch_height);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	public:
		virtual bool surface_ready() = 0;
		virtual bool ui_thread_ready() = 0;
		virtual bool shown();

	public:
		void enter_critical_section();
		void leave_critical_section();

	internal:
		virtual void refresh(WarGrey::SCADA::IPlanet* target) = 0;

	internal:
		WarGrey::SCADA::Syslog* get_logger() override;

	private:
		WarGrey::SCADA::Syslog* logger;
		std::mutex section;

	private:
		DisplayFit mode;
		float target_width;
		float target_height;
		float source_width;
		float source_height;
    };

	private ref class UniverseDisplay : public WarGrey::SCADA::IDisplay {
	public:
		virtual ~UniverseDisplay();

	internal:
		UniverseDisplay(WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::IPlanet* first_planet = nullptr,
			Windows::UI::Xaml::Controls::ListView^ navigator = nullptr);

		UniverseDisplay(WarGrey::SCADA::DisplayFit mode,
			float dest_width, float dest_height,
			WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::IPlanet* first_planet = nullptr,
			Windows::UI::Xaml::Controls::ListView^ navigator = nullptr);

		UniverseDisplay(WarGrey::SCADA::DisplayFit mode,
			float dest_width, float dest_height,
			float src_width, float src_height,
			WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::IPlanet* first_planet = nullptr,
			Windows::UI::Xaml::Controls::ListView^ navigator = nullptr);
		
	internal:
		void refresh(WarGrey::SCADA::IPlanet* target) override;
		read_only_property(WarGrey::SCADA::IPlanet*, current_planet);

	public:
		read_only_property(Windows::UI::Xaml::Controls::Primitives::Selector^, navigator);
		read_only_property(unsigned int, current_planet_index);

	public:
		override_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);
		override_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		override_read_only_property(float, actual_width);
		override_read_only_property(float, actual_height);
		bool surface_ready() override;
		bool ui_thread_ready() override;

	public: // weird, `UniverseDisplay` cannot do it own its own
		void register_virtual_keydown_event_handler(Windows::UI::Xaml::UIElement^ target);
		void disable_predefined_shortcuts(bool yes);

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);
		void transfer(int delta_idx, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_to(int idx, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_previous(unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_next(unsigned int timeline_ms = 0, unsigned int frame_count = 4);

	public:
		void on_elapsed(long long count, long long interval, long long uptime) override;

	protected private:
		virtual void construct() {}
		void add_planet(WarGrey::SCADA::IPlanet* planet);
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual void collapse();
		
	private:
		void do_refresh(Platform::Object^ sender, Platform::Object^ args);
		void do_transfer(Platform::Object^ sender, Windows::UI::Xaml::Controls::ItemClickEventArgs^ args);
		void do_resize(Platform::Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ args);
		
		void do_construct(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
			Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);

		void do_paint(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);

	private:
		void on_pointer_pressed(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_moveout(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_released(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_translating_x();
		
	private:
		void on_key(Platform::Object^ sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs^ args);
		void on_character(Windows::UI::Core::CoreWindow^ sender, Windows::UI::Core::CharacterReceivedEventArgs^ args);

	private:
		Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ display;
		Windows::UI::Xaml::Controls::ListView^ navigator_view;
		WarGrey::SCADA::IPlanet* head_planet;
		WarGrey::SCADA::IPlanet* recent_planet;

	private:
		std::map<unsigned int, Windows::UI::Input::PointerUpdateKind> figures;
		float figure_x0;
		float figure_x;

	private:
		Windows::UI::Xaml::DispatcherTimer^ transfer_clock;
		WarGrey::SCADA::IPlanet* from_planet;
		float transfer_delta;
		float transferX;

	private:
		bool shortcuts_enabled;
	};
}
