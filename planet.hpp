#pragma once

#include <list>

#include "datum/credit.hpp"

#include "universe.hxx"

#include "decorator/decorator.hpp"
#include "virtualization/screen.hpp"

namespace WarGrey::SCADA {
	private class IPlanetInfo abstract {
	public:
		virtual ~IPlanetInfo() noexcept {}
		IPlanetInfo(IScreen* master) : master(master) {}
		
	public:
		IScreen* master;
	};

	/** Note
	 * The destruction of `IPlanet` is always performed by its `display`
	 *  since its instance cannot belong to multiple `display`s.
	 *
	 *  Do not `delete` it on your own.
	 */
	private class IPlanet abstract {
	public:
		virtual ~IPlanet() noexcept;
		IPlanet(Platform::String^ name);

	public:
		Platform::String^ name();
		Platform::String^ display_name();
		WarGrey::SCADA::IScreen* master();
		WarGrey::SCADA::Syslog* get_logger();

	public:
		bool shown();
		bool ui_thread_ready();
		float actual_width();
		float actual_height();
		float min_width();
		float min_height();

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
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float X, float Y, float Width, float Height) {}
		virtual void collapse();
		
	public:
		virtual WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) = 0;
		virtual bool fill_graphlet_location(IGraphlet* g, float* x, float* y, float fx = 0.0F, float fy = 0.0F) = 0;
		virtual bool fill_graphlet_boundary(IGraphlet* g, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_graphlets_boundary(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(IGraphlet* g, float x = 0.0F, float y = 0.0F, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void insert(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void insert(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void move(IGraphlet* g, float x, float y) = 0;
		virtual void move_to(IGraphlet* g, float x, float y, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void move_to(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void move_to(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) = 0;
		virtual void remove(IGraphlet* g) = 0;
		virtual void erase() = 0;

	public:
		virtual void translate(float x, float y) = 0;
		virtual void scale(float sx, float sy = 0.0F) = 0;

	public: // TODO: find a way to get the real background of the underneath controller such as the `Flyout` instance.
		virtual void set_background(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, float corner_radius = 0.0F) = 0;
		virtual void cellophane(IGraphlet* g, float opacity) = 0;

	public:
		virtual void notify_graphlet_ready(IGraphlet* g) = 0;
		virtual void on_graphlet_ready(IGraphlet* g) = 0;
		
	public:
		virtual void on_focus(WarGrey::SCADA::IGraphlet* g, bool on_off) {}
		virtual bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) { return false; }
		virtual bool on_character(unsigned int keycode) { return false; }
		virtual void on_elapse(long long count, long long interval, long long uptime) {}
		virtual void on_elapse(long long count, long long interval, long long uptime, long long elapsed) {}
		virtual void on_hover(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) {}
		virtual void on_goodbye(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) {}
		virtual void on_swipe(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) {}
		virtual void on_tap(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) {}
		virtual void on_tap_selected(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) {}
		virtual void on_gesture(std::list<Windows::Foundation::Numerics::float2>& anchors, float planet_x, float planet_y) {}
		virtual void on_transfer(WarGrey::SCADA::IPlanet* from, WarGrey::SCADA::IPlanet* to) {}

	public:
		virtual void draw_visible_selection(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float X, float Y, float width, float height) = 0;
		virtual IGraphlet* find_next_selected_graphlet(IGraphlet* start = nullptr) = 0;
		virtual void add_selected(IGraphlet* g) = 0;
		virtual void set_selected(IGraphlet* g) = 0;
		virtual void no_selected() = 0;
		virtual unsigned int count_selected() = 0;
		virtual bool is_selected(IGraphlet* g) = 0;

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
		virtual bool on_pointer_moved(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

	public:
		Windows::Foundation::Point global_to_local_point(IGraphlet* g, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Point local_to_global_point(IGraphlet* g, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F);

	public:
		void begin_update_sequence();
		bool in_update_sequence();
		void end_update_sequence();
		bool needs_update();
		void notify_graphlet_updated(ISprite* g);

	public:
		void enter_critical_section();
		void enter_shared_section();
		void leave_critical_section();
		void leave_shared_section();

	public:
		void save_logo(float logo_width = 0.0F, float logo_height = 0.0F, Platform::String^ path = nullptr, float dpi = 96.0F);
		
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float width, float height,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ bgcolor = nullptr, float dpi = 96.0F);
		
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float x, float y, float width, float height,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ bgcolor = nullptr, float dpi = 96.0F);

		void save(Platform::String^ path, float width, float height,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ bgcolor = nullptr, float dpi = 96.0F);

		void save(Platform::String^ path, float x, float y, float width, float height,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ bgcolor = nullptr, float dpi = 96.0F);

	public:
		bool fill_graphlet_location(IGraphlet* g, float* x, float* y, GraphletAnchor a);
		void insert(IGraphlet* g, float x, float y, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void insert(IGraphlet* g, IGraphlet* tg, GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void insert(IGraphlet* g, IGraphlet* tg, GraphletAnchor ta, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F);
		void insert(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void insert(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void move_to(IGraphlet* g, float x, float y, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void move_to(IGraphlet* g, IGraphlet* tg, GraphletAnchor ta, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void move_to(IGraphlet* g, IGraphlet* tg, GraphletAnchor ta, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F);
		void move_to(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);
		void move_to(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F);

	public:
		template<class G>
		G* insert_one(G* g, float x = 0.0F, float y = 0.0F, GraphletAnchor a = GraphletAnchor::LT) {
			this->insert(g, x, y, a);

			return g;
		}

		template<class G, typename E>
		Credit<G, E>* insert_one(Credit<G, E>* g, E id, float x = 0.0F, float y = 0.0F, GraphletAnchor a = GraphletAnchor::LT) {
			g->id = id;

			return this->insert_one(g, x, y, a);
		}

		template<class G, typename GID, typename EID>
		GroupCredit<G, GID, EID>* insert_one(GroupCredit<G, GID, EID>* g, GID gid, EID id
			, float x = 0.0F, float y = 0.0F, GraphletAnchor a = GraphletAnchor::LT) {
			g->gid = gid;
			g->id = id;

			return this->insert_one(g, x, y, a);
		}

	public:
		IPlanetInfo* info;

	private:
		Platform::String^ caption;
    };

	private class Planet : public WarGrey::SCADA::IPlanet {
	public:
		virtual ~Planet() noexcept;
		Planet(Platform::String^ caption, unsigned int initial_mode = 0);

	public:
		void change_mode(unsigned int mode); // NOTE: mode 0 is designed for UI graphlets which will be unmasked in all modes;
		unsigned int current_mode();

		bool graphlet_unmasked(WarGrey::SCADA::IGraphlet* g);
		void push_decorator(WarGrey::SCADA::IPlanetDecorator* decorator);

    public:
        void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float X, float Y, float Width, float Height) override;

    public: // learn C++ "Name Hiding"
		using WarGrey::SCADA::IPlanet::fill_graphlet_location;
		using WarGrey::SCADA::IPlanet::insert;
		using WarGrey::SCADA::IPlanet::move_to;

		WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) override;
		bool fill_graphlet_location(IGraphlet* g, float* x, float* y, float fx = 0.0F, float fy = 0.0F) override;
		bool fill_graphlet_boundary(IGraphlet* g, float* x, float* y, float* width, float* height) override;
		void fill_graphlets_boundary(float* x, float* y, float* width, float* height) override;
		void insert(IGraphlet* g, float x = 0.0F, float y = 0.0F, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void insert(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void insert(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void move(IGraphlet* g, float x, float y) override;
		void move_to(IGraphlet* g, float x, float y, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void move_to(IGraphlet* g, IGraphlet* tg, float tfx, float tfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void move_to(IGraphlet* g, IGraphlet* xtg, float xfx, IGraphlet* ytg, float yfy, float fx = 0.0F, float fy = 0.0F, float dx = 0.0F, float dy = 0.0F) override;
		void remove(IGraphlet* g) override;
		void erase() override;
		void size_cache_invalid();

	public:
		void translate(float x, float y) override;
		void scale(float sx, float sy = 0.0F) override;

	public:
		virtual void set_background(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, float corner_radius = 0.0F) override;
		void cellophane(IGraphlet* g, float opacity) override;

	public:
		void notify_graphlet_ready(IGraphlet* g) override;
		void on_graphlet_ready(IGraphlet* g) {}
		
	public:
		using WarGrey::SCADA::IPlanet::on_elapse;

		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		bool on_character(unsigned int keycode) override;
		void on_swipe(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) override;
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y) override;
		void on_elapse(long long count, long long interval, long long uptime) override;

	public:
		void draw_visible_selection(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float width, float height) override;
		IGraphlet* find_next_selected_graphlet(IGraphlet* start = nullptr) override;
		void add_selected(IGraphlet* g) override;
        void set_selected(IGraphlet* g) override;
        void no_selected() override;
		unsigned int count_selected() override;
		bool is_selected(IGraphlet* g) override;

	public:
		WarGrey::SCADA::IGraphlet* get_focus_graphlet() override;
		void set_caret_owner(IGraphlet* g) override;
		void hide_virtual_keyboard();
		void show_virtual_keyboard(ScreenKeyboard type, GraphletAnchor a = GraphletAnchor::RB, float dx = 0.0F, float dy = 0.0F);
		void show_virtual_keyboard(ScreenKeyboard type, IGraphlet* g, GraphletAnchor a = GraphletAnchor::RB, float dx = 0.0F, float dy = 0.0F);
		void show_virtual_keyboard(ScreenKeyboard type, float x, float y, float dx = 0.0F, float dy = 0.0F);

	public:
		bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
			override;

		bool on_pointer_moved(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
			override;

		bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
			override;

		bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
			override;

    private:
		void switch_virtual_keyboard(WarGrey::SCADA::ScreenKeyboard type);
        void recalculate_graphlets_extent_when_invalid();
		bool say_goodbye_to_hover_graphlet(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk);

    private:
#ifdef _DEBUG
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ figure_track;
#endif
		std::list<Windows::Foundation::Numerics::float2> figure_anchors;
		float track_thickness;

    private:
        float graphlets_left;
        float graphlets_top;
        float graphlets_right;
        float graphlets_bottom;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		float background_corner_radius;

    private:
        std::list<WarGrey::SCADA::IPlanetDecorator*> decorators;
        WarGrey::SCADA::IGraphlet* head_graphlet;
		WarGrey::SCADA::IGraphlet* focused_graphlet;
		WarGrey::SCADA::IGraphlet* hovering_graphlet; // not used when PointerDeviceType::Touch
		unsigned int mode;

	private:
		WarGrey::SCADA::IKeyboard* keyboard;
		WarGrey::SCADA::IKeyboard* numpad;
		WarGrey::SCADA::IKeyboard* arrowpad;
		WarGrey::SCADA::IKeyboard* bucketpad;
		float keyboard_x;
		float keyboard_y;

	private:
		float translate_x;
		float translate_y;
		float scale_x;
		float scale_y;
    };

	private class IHeadUpPlanet abstract : public WarGrey::SCADA::Planet {
	public:
		IHeadUpPlanet(Platform::String^ caption, unsigned int initial_mode = 0);

	public:
		virtual void fill_margin(float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr);
	};
}
