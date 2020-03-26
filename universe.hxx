#pragma once

#include <map>
#include <mutex>

#include "navigator/navigator.hpp"
#include "virtualization/screen.hpp"
#include "datum/class.hpp"

#include "display.hxx"
#include "timer.hxx"

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

	private ref class UniverseDisplay : public WarGrey::SCADA::IDisplay {
	public:
		virtual ~UniverseDisplay();

	internal:
		UniverseDisplay(WarGrey::GYDM::Syslog* logger = nullptr,
			Platform::String^ setting_name = nullptr,
			WarGrey::SCADA::IUniverseNavigator* navigator = nullptr,
			WarGrey::SCADA::IHeadUpPlanet* heads_up_planet = nullptr);

		UniverseDisplay(WarGrey::SCADA::DisplayFit mode,
			float dest_width, float dest_height,
			WarGrey::GYDM::Syslog* logger = nullptr,
			Platform::String^ setting_name = nullptr,
			WarGrey::SCADA::IUniverseNavigator* navigator = nullptr,
			WarGrey::SCADA::IHeadUpPlanet* heads_up_planet = nullptr);

		UniverseDisplay(WarGrey::SCADA::DisplayFit mode,
			float dest_width, float dest_height,
			float src_width, float src_height,
			WarGrey::GYDM::Syslog* logger = nullptr,
			Platform::String^ setting_name = nullptr,
			WarGrey::SCADA::IUniverseNavigator* navigator = nullptr,
			WarGrey::SCADA::IHeadUpPlanet* heads_up_planet = nullptr);
		
	internal:
		void refresh(WarGrey::SCADA::IPlanet* target) override;
		read_only_property(WarGrey::SCADA::IPlanet*, current_planet);
		read_only_property(WarGrey::SCADA::IHeadUpPlanet*, heads_up_planet);
		read_only_property(WarGrey::SCADA::IUniverseNavigator*, navigator);
		read_only_property(int, current_planet_index);

	public:
		override_read_only_property(Windows::UI::Xaml::Controls::UserControl^, canvas);
		override_read_only_property(Microsoft::Graphics::Canvas::CanvasDevice^, device);
		override_read_only_property(float, actual_width);
		override_read_only_property(float, actual_height);
		read_write_property(double, global_mask_alpha);
		read_write_property(double, mask_alpha);

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F) override;
		void use_global_mask_setting(bool yes, bool* prev_state = nullptr);
		bool surface_ready() override;
		bool ui_thread_ready() override;

	public: // weird, `UniverseDisplay` cannot do it own its own
		void register_virtual_keydown_event_handler(Windows::UI::Xaml::UIElement^ target);
		void disable_predefined_shortcuts(bool yes);

	public:
		void transfer(int delta_idx, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_to(Platform::String^ name, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_to(int idx, unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_previous(unsigned int timeline_ms = 0, unsigned int frame_count = 4);
		void transfer_next(unsigned int timeline_ms = 0, unsigned int frame_count = 4);

	public:
		void on_elapse(long long count, long long interval, long long uptime) override;
		void on_elapse(long long count, long long interval, long long uptime, long long elapsed) override;
		void on_navigate(int from_index, int to_index) override;
	
	internal:
		Windows::Foundation::Point global_to_local_point(IPlanet* p, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F) override;
		Windows::Foundation::Point local_to_global_point(IPlanet* p, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F) override;
		virtual float planet_actual_width(IPlanet* p) override;
		virtual float planet_actual_height(IPlanet* p) override;

	protected private:
		virtual void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason) {}
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) { return false; }
		virtual bool on_character(unsigned int keycode) { return false; }
		
	protected private:
		void push_planet(WarGrey::SCADA::IPlanet* planet);
		void collapse();
		
	private:
		void do_refresh(Platform::Object^ sender, Platform::Object^ args);
		void do_resize(Platform::Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ args);
		
		void do_construct(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
			Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);

		void do_paint(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);

	private:
		void on_virtual_key(Platform::Object^ sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs^ args);
		void on_keycode(Windows::UI::Core::CoreWindow^ sender, Windows::UI::Core::CharacterReceivedEventArgs^ args);
		void on_pointer_pressed(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_moveout(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_released(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_pointer_wheel(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		void on_translating_x(float delta);

	private:
		void notify_transfer(WarGrey::SCADA::IPlanet* from, WarGrey::SCADA::IPlanet* to);

	private:
		Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ display;
		WarGrey::SCADA::IScreen* screen;
		WarGrey::SCADA::IUniverseNavigator* _navigator;
		WarGrey::SCADA::IPlanet* head_planet;
		WarGrey::SCADA::IPlanet* recent_planet;

	private:
		WarGrey::SCADA::IHeadUpPlanet* headup_planet;
		float hup_top_margin;
		float hup_bottom_margin;
		float hup_left_margin;
		float hup_right_margin;

	private:
		std::map<unsigned int, WarGrey::SCADA::UniverseFigure> figures;
		Windows::Foundation::Numerics::float2 gesture_lt;
		Windows::Foundation::Numerics::float2 gesture_rb;

	private:
		Windows::Storage::ApplicationDataContainer^ universe_settings;
		Windows::UI::Xaml::DispatcherTimer^ transfer_clock;
		WarGrey::SCADA::IPlanet* from_planet;
		float transfer_delta;
		float transferX;
		float transferY;

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ mask_color;
		bool follow_global_mask_setting;

	private:
		bool shortcuts_enabled;
	};
}
