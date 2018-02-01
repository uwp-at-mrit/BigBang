#pragma once

#include <shared_mutex>

#include "universe.hxx"
#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	typedef Windows::Foundation::Collections::IVector<Windows::UI::Input::PointerPoint^> VectorOfPointerPoint;

	private class IPlanetInfo abstract {
	public:
		virtual ~IPlanetInfo() noexcept {};
		IPlanetInfo(IDisplay^ master) : master(master) {};
		
	public:
		IDisplay^ master;
	};

    private class IPlanet abstract {
    public:
		virtual ~IPlanet() noexcept;
		IPlanet(Platform::String^ name) : caption(name) {};

	public:
		Platform::String^ name() { return this->caption; }
		Platform::Object^ navigation_label() { return this->caption; }

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float width, float height, float dpi = 96.0);
		void save(Platform::String^ path, float width, float height, float dpi = 96.0);

	public:
		Windows::Foundation::Point global_to_local_point(ISnip* snip, float global_x, float global_y, float xoff = 0.0F, float yoff = 0.0F);
		Windows::Foundation::Point local_to_global_point(ISnip* snip, float local_x, float local_y, float xoff = 0.0F, float yoff = 0.0F);
		void fill_actual_extent(float* width, float* height);
		
    public:
        virtual void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {};
		virtual void reflow(float width, float height) {}; 
		virtual void update(long long count, long long interval, long long uptime, bool is_slow) {};
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args, float Width, float Height) {};
		virtual void collapse() {};

	public:
		virtual WarGrey::SCADA::ISnip* find_snip(float x, float y) = 0;
		virtual void fill_snip_location(ISnip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;
		virtual void fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) = 0;
		virtual void fill_snips_bounds(float* x, float* y, float* width, float* height) = 0;
		virtual void insert(ISnip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) = 0;
		virtual void move(ISnip* snip, float x, float y) = 0;
		virtual void move_to(ISnip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) = 0;

	public:
		virtual void on_tap(WarGrey::SCADA::ISnip* snip, float local_x, float local_y, bool shifted, bool controled) {};
		virtual void on_right_tap(WarGrey::SCADA::ISnip* snip, float local_x, float local_y, bool shifted, bool controled) {};

	public:
		virtual void add_selected(ISnip* snip) = 0;
		virtual void set_selected(ISnip* snip) = 0;
		virtual void no_selected() = 0;

	public:
		virtual bool can_interactive_move(ISnip* snip, float local_x, float local_y) { return false; }
		virtual bool can_select(ISnip* snip) { return false; }
		virtual bool can_select_multiple() { return false; }
		virtual void before_select(ISnip* snip, bool on_or_off) {}
		virtual void after_select(ISnip* snip, bool on_or_off) {}

    public:
		void enter_critical_section();
		void enter_shared_section();
		void leave_critical_section();
		void leave_shared_section();

	public:
		virtual bool on_pointer_moved(float x, float y,
			WarGrey::SCADA::VectorOfPointerPoint^ pts,
			Windows::UI::Input::PointerUpdateKind puk,
			Windows::System::VirtualKeyModifiers vkms)
		{ return false; }

		virtual bool on_pointer_pressed(float x, float y,
			Windows::UI::Input::PointerUpdateKind puk,
			Windows::System::VirtualKeyModifiers vkms)
		{ return false; }

		virtual bool on_pointer_released(float x, float y,
			Windows::UI::Input::PointerUpdateKind puk,
			Windows::System::VirtualKeyModifiers vkms)
		{ return false; }

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
		 * mode 0 is designed for UI snips which will be unmasked in all modes;
		 * however, only UI snips are unmasked in mode 0.
		 */
		void change_mode(unsigned int mode);
		bool snip_unmasked(WarGrey::SCADA::ISnip* snip);

    public:
        void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;
		void collapse() override;

    public:
        void fill_snips_bounds(float* x, float* y, float* width, float* height);
        void size_cache_invalid();

    public:
        void set_decorator(WarGrey::SCADA::IPlanetDecorator* decorator);

    public:
		WarGrey::SCADA::ISnip* find_snip(float x, float y) override;
        void fill_snip_location(ISnip* snip, float* x, float* y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;
		void fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) override;
		void insert(ISnip* snip, double degrees = 0.0, float x = 0.0F, float y = 0.0F) override;
        void move(ISnip* snip, float x, float y) override;
        void move_to(ISnip* snip, float x, float y, WarGrey::SCADA::SnipCenterPoint cp = SnipCenterPoint::LT) override;

	public:
		void on_tap(WarGrey::SCADA::ISnip* snip, float x, float y, bool shifted, bool controled) override;

	public:
        void add_selected(ISnip* snip) override;
        void set_selected(ISnip* snip) override;
        void no_selected() override;

	public:
		bool on_pointer_pressed(float x, float y, Windows::UI::Input::PointerUpdateKind puk, Windows::System::VirtualKeyModifiers vkms) override;
		bool on_pointer_moved(float x, float y, VectorOfPointerPoint^ pts, Windows::UI::Input::PointerUpdateKind puk, Windows::System::VirtualKeyModifiers vkms) override;
		bool on_pointer_released(float x, float y, Windows::UI::Input::PointerUpdateKind puk, Windows::System::VirtualKeyModifiers vkms) override;

    private:
        void recalculate_snips_extent_when_invalid();

    private:
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
        WarGrey::SCADA::IPlanetDecorator* decorator;
        WarGrey::SCADA::ISnip* head_snip;
		unsigned int mode;
    };
}
