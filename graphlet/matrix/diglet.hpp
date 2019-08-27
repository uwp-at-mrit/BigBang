#pragma once

#include <deque>
#include <map>

#include "graphlet/msappdatalet.hxx"
#include "graphlet/planetlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private ref class DigMap sealed {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::DigMap^>^ load_async(Platform::String^ dig);

	public:
		virtual ~DigMap();

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height);

	internal:
		void push_back_item(WarGrey::SCADA::IDigDatum* item);
		void rewind();
		WarGrey::SCADA::IDigDatum* step();
		
	private:
		DigMap();

	private:
		double lx;
		double ty;
		double rx;
		double by;

	private:
		std::deque<WarGrey::SCADA::IDigDatum*> items;
		std::deque<WarGrey::SCADA::IDigDatum*>::iterator cursor;
		std::map<WarGrey::SCADA::DigDatumType, unsigned int> counters;
	};

	private class Diglet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::DigMap, WarGrey::SCADA::Planetlet, int> {
	public:
		virtual ~Diglet() noexcept;

		Diglet(Platform::String^ file_bmp, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "dig");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		bool on_character(unsigned int keycode) override;

	protected:
		void on_appdata(Windows::Foundation::Uri^ ms_appdata_dig, WarGrey::SCADA::DigMap^ doc_dig, int hint) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ ms_appdata_dig, int hint) override {}

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::DigMap^ graph_dig;
		
	private:
		Windows::Foundation::Uri^ ms_appdata_dig;

	private:
		WarGrey::SCADA::IGraphlet* map;
		std::deque<Platform::Object^> icons;
		float view_width;
		float view_height;
	};
}
