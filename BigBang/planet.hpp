#pragma once

#include <list>
#include <shared_mutex>

#include "universe.hxx"
#include "decorator/decorator.hpp"
#include "virtualization/numpad.hpp"

namespace WarGrey::SCADA {
	typedef Windows::Foundation::Collections::IVector<Windows::UI::Input::PointerPoint^> VectorOfPointerPoint;

	private class IPlanetInfo abstract {
	public:
		virtual ~IPlanetInfo() noexcept {}
		IPlanetInfo(IDisplay^ master) : master(master) {}
		
	public:
		IDisplay^ master;
	};

	private class IPlanet abstract {
	public:
		virtual ~IPlanet() noexcept;
		IPlanet(Platform::String^ name);

	public:
		bool shown();
		bool ui_thread_ready();
		float actual_width();
		float actual_height();

	public:
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	public:
		virtual void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {}
		virtual void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {}
		virtual void reflow(float width, float height) {}
		virtual bool surface_ready();
		virtual void notify_surface_ready() {}
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float Width, float Height) {}
		virtual void collapse() {}

	public:
		virtual WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) = 0;
		virtual bool fill_graphlet_location(IGraphlet* g, float* x, float* y, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) = 0;
		virtual bool fill_graphlet_boundary(IGraphlet* g, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_graphlets_boundary(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(IGraphlet* g, float x = 0.0F, float y = 0.0F, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) = 0;
		virtual void insert(IGraphlet* g, IGraphlet* target, WarGrey::SCADA::GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void move(IGraphlet* g, float x, float y) = 0;
		virtual void move_to(IGraphlet* g, float x, float y, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) = 0;
		virtual void move_to(IGraphlet* g, IGraphlet* target, WarGrey::SCADA::GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F) = 0;

	public:
		virtual void notify_graphlet_updated(ISprite* g) = 0;
		virtual void notify_graphlet_ready(IGraphlet* g) = 0;
		virtual void on_graphlet_ready(IGraphlet* g) = 0;
		virtual void begin_update_sequence() = 0;
		virtual bool in_update_sequence() = 0;
		virtual void end_update_sequence() = 0;

	public:
		virtual bool on_char(Windows::System::VirtualKey key) { return false; }
		virtual void on_elapse(long long count, long long interval, long long uptime) {}
		virtual void on_hover(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y, bool shifted, bool controled) {}
		virtual void on_goodbye(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y, bool shifted, bool controled) {}
		virtual void on_tap(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y, bool shifted, bool controled) {}
		virtual void on_right_tap(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y, bool shifted, bool controled) {}

	public:
		virtual void draw_visible_selection(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float x, float y, float width, float height) = 0;
		virtual void add_selected(IGraphlet* g) = 0;
		virtual void set_selected(IGraphlet* g) = 0;
		virtual void no_selected() = 0;

	public:
		virtual bool can_interactive_move(IGraphlet* g, float local_x, float local_y) { return false; }
		virtual bool can_select(IGraphlet* g) { return true; }
		virtual bool can_select_multiple() { return false; }
		virtual void before_select(IGraphlet* g, bool on_or_off) {}
		virtual void after_select(IGraphlet* g, bool on_or_off) {}
		
	public:
		virtual WarGrey::SCADA::IGraphlet* get_focus_graphlet() = 0;
		virtual void set_caret_owner(IGraphlet* g) = 0;

	public:
		virtual bool on_pointer_moved(float x, float y, WarGrey::SCADA::VectorOfPointerPoint^ pts,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

		virtual bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

		virtual bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

		virtual bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

	public:
		Platform::String^ name();
		Platform::Object^ navigation_label();
		WarGrey::SCADA::Syslog* get_logger();

	public:
		Windows::Foundation::Point global_to_local_point(IGraphlet* g, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Point local_to_global_point(IGraphlet* g, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F);

	public:
		void enter_critical_section();
		void enter_shared_section();
		void leave_critical_section();
		void leave_shared_section();

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float width, float height, float dpi = 96.0);
		void save(Platform::String^ path, float width, float height, float dpi = 96.0);

	public:
		template<class G, unsigned int N>
		void insert_all(G* (&gs)[N], bool reversed = false) {
			if (reversed) {
				for (unsigned int idx = N; idx > 0; idx--) {
					if (gs[idx - 1] != nullptr) this->insert(gs[idx - 1]);
				}
			} else {
				for (unsigned int idx = 0; idx < N; idx++) {
					if (gs[idx] != nullptr) this->insert(gs[idx]);
				}
			}
		}

		template<class G>
		G* insert_one(G* g, float x = 0.0F, float y = 0.0F, GraphletAnchor a = GraphletAnchor::LT) {
			this->insert(g, x, y, a);

			return g;
		}

	public:
		IPlanetInfo* info;

	private:
		Platform::String^ caption;
		std::shared_mutex section;
    };

	private class Planet : public WarGrey::SCADA::IPlanet {
	public:
		~Planet() noexcept;
		Planet(Platform::String^ caption, unsigned int initial_mode = 0);

	public:
		void change_mode(unsigned int mode); // NOTE: mode 0 is designed for UI graphlets which will be unmasked in all modes;
		bool graphlet_unmasked(WarGrey::SCADA::IGraphlet* g);
		void append_decorator(WarGrey::SCADA::IPlanetDecorator* decorator);

    public:
        void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) override;
        void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;
		void collapse() override;

    public:
		WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) override;
        bool fill_graphlet_location(IGraphlet* g, float* x, float* y, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) override;
		bool fill_graphlet_boundary(IGraphlet* g, float* x, float* y, float* width, float* height) override;
		void fill_graphlets_boundary(float* x, float* y, float* width, float* height);
		void insert(IGraphlet* g, float x = 0.0F, float y = 0.0F, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) override;
		void insert(IGraphlet* g, IGraphlet* target, WarGrey::SCADA::GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F) override;
		void move(IGraphlet* g, float x, float y) override;
		void move_to(IGraphlet* g, float x, float y, WarGrey::SCADA::GraphletAnchor a = GraphletAnchor::LT) override;
		void move_to(IGraphlet* g, IGraphlet* target, WarGrey::SCADA::GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F) override;
		void size_cache_invalid();

	public:
		void notify_graphlet_updated(ISprite* g) override;
		void notify_graphlet_ready(IGraphlet* g) override;
		void on_graphlet_ready(IGraphlet* g) override {}
		void begin_update_sequence() override;
		bool in_update_sequence() override;
		void end_update_sequence() override;

	public:
		bool on_char(Windows::System::VirtualKey key) override;
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y, bool shifted, bool controled) override;

	public:
		void draw_visible_selection(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float x, float y, float width, float height) override;
        void add_selected(IGraphlet* g) override;
        void set_selected(IGraphlet* g) override;
        void no_selected() override;

	public:
		WarGrey::SCADA::IGraphlet* get_focus_graphlet() override;
		void set_caret_owner(IGraphlet* g) override;
		void show_virtual_keyboard(ScreenKeyboard type);
		void show_virtual_keyboard(ScreenKeyboard type, float x, float y);

	public:
		bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled) override;

		bool on_pointer_moved(float x, float y, WarGrey::SCADA::VectorOfPointerPoint^ pts,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled) override;

		bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled) override;

		bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled) override;

    private:
        void recalculate_graphlets_extent_when_invalid();
		bool say_goodbye_to_the_hovering_graphlet(float x, float y, bool shifted, bool ctrled);

    private:
        float last_pointer_x;
        float last_pointer_y;
        float rubberband_x[2];
        float* rubberband_y;
        bool rubberband_allowed;

    private:
        float graphlets_left;
        float graphlets_top;
        float graphlets_right;
        float graphlets_bottom;
        float preferred_min_width;
        float preferred_min_height;

    private:
        std::list<WarGrey::SCADA::IPlanetDecorator*> decorators;
        WarGrey::SCADA::IGraphlet* head_graphlet;
		WarGrey::SCADA::IGraphlet* focused_graphlet;
		WarGrey::SCADA::IGraphlet* hovering_graphlet; // not used when PointerDeviceType::Touch
		unsigned int mode;

	private:
		WarGrey::SCADA::Numpad* numpad;
		float keyboard_x;
		float keyboard_y;

	private:
		int update_sequence_depth;
		bool needs_update;
    };
}
