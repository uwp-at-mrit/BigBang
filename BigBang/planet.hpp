#pragma once

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
		IPlanet(Platform::String^ name) : caption(name) {}

	public:
		virtual void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {}
		virtual void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {}
		virtual void reflow(float width, float height) {}
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float Width, float Height) {}
		virtual void collapse() {}

	public:
		virtual WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) = 0;
		virtual bool fill_graphlet_location(IGraphlet* g, float* x, float* y, WarGrey::SCADA::GraphletAlignment cp = GraphletAlignment::LT) = 0;
		virtual bool fill_graphlet_bound(IGraphlet* g, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_graphlets_bounds(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(IGraphlet* g, double degrees = 0.0, float x = 0.0F, float y = 0.0F) = 0;
		virtual void move(IGraphlet* g, float x, float y) = 0;
		virtual void move_to(IGraphlet* g, float x, float y, WarGrey::SCADA::GraphletAlignment cp = GraphletAlignment::LT) = 0;

	public:
		virtual bool on_char(Windows::System::VirtualKey key) { return false; }
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
		virtual bool on_pointer_moved(float x, float y,
			WarGrey::SCADA::VectorOfPointerPoint^ pts,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

		virtual bool on_pointer_pressed(float x, float y,
			Windows::UI::Input::PointerUpdateKind puk,
			bool shifted, bool ctrled)
		{ return false; }

		virtual bool on_pointer_released(float x, float y,
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
		void fill_actual_extent(float* width, float* height);
		
	public:
		template<class G, unsigned int N>
		void insert_all(G* (&gs)[N], unsigned int start_idx = 0) {
			for (unsigned int idx = start_idx; idx < N; idx++) {
				if (gs[idx] != nullptr) {
					this->insert(gs[idx]);
				}
			}
		}

		template<class G>
		G* insert_one(G* g, double degrees = 0.0, float x = 0.0F, float y = 0.0F) {
			this->insert(g);

			return g;
		}

	public:
		void enter_critical_section();
		void enter_shared_section();
		void leave_critical_section();
		void leave_shared_section();

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float width, float height, float dpi = 96.0);
		void save(Platform::String^ path, float width, float height, float dpi = 96.0);

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
		/** NOTE
		 * mode 0 is designed for UI graphlets which will be unmasked in all modes;
		 * however, only UI graphlets are unmasked in mode 0.
		 */
		void change_mode(unsigned int mode);
		bool graphlet_unmasked(WarGrey::SCADA::IGraphlet* g);
		void set_decorator(WarGrey::SCADA::IPlanetDecorator* decorator);

    public:
        void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) override;
        void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;
		void collapse() override;

    public:
		WarGrey::SCADA::IGraphlet* find_graphlet(float x, float y) override;
        bool fill_graphlet_location(IGraphlet* g, float* x, float* y, WarGrey::SCADA::GraphletAlignment cp = GraphletAlignment::LT) override;
		bool fill_graphlet_bound(IGraphlet* g, float* x, float* y, float* width, float* height) override;
		void fill_graphlets_bounds(float* x, float* y, float* width, float* height);
		void insert(IGraphlet* g, double degrees = 0.0, float x = 0.0F, float y = 0.0F) override;
        void move(IGraphlet* g, float x, float y) override;
        void move_to(IGraphlet* g, float x, float y, WarGrey::SCADA::GraphletAlignment cp = GraphletAlignment::LT) override;
		void size_cache_invalid();

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
		bool on_pointer_pressed(float x, float y, Windows::UI::Input::PointerUpdateKind puk, bool shifted, bool ctrled) override;
		bool on_pointer_moved(float x, float y, VectorOfPointerPoint^ pts, Windows::UI::Input::PointerUpdateKind puk, bool shifted, bool ctrled) override;
		bool on_pointer_released(float x, float y, Windows::UI::Input::PointerUpdateKind puk, bool shifted, bool ctrled) override;

    private:
        void recalculate_graphlets_extent_when_invalid();

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
        WarGrey::SCADA::IPlanetDecorator* decorator;
        WarGrey::SCADA::IGraphlet* head_graphlet;
		WarGrey::SCADA::IGraphlet* focus_graphlet;
		WarGrey::SCADA::IGraphlet* hover_graphlet;
		unsigned int mode;

	private:
		WarGrey::SCADA::Numpad* numpad;
		float keyboard_x;
		float keyboard_y;
    };
}
