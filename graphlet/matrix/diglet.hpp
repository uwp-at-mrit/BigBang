#pragma once

#include <deque>
#include <map>

#include "graphlet/msappdatalet.hxx"
#include "graphlet/planetlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private ref class DigVectorMap sealed {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::DigVectorMap^>^ load_async(Platform::String^ dig);

	public:
		virtual ~DigVectorMap();

	public:
		void fill_enclosing_box(double* x, double* y, double* width, double* height);

	internal:
		void push_back_item(WarGrey::SCADA::IDigDatum* item);
		void rewind();
		WarGrey::SCADA::IDigDatum* step();
		
	private:
		DigVectorMap();

	private:
		double lx;
		double ty;
		double rx;
		double by;

	private:
		std::deque<WarGrey::SCADA::IDigDatum*> items;
		std::deque<WarGrey::SCADA::IDigDatum*>::iterator items_it;
		std::map<WarGrey::SCADA::DigDatumType, unsigned int> counters;
	};

	private class Diglet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::DigVectorMap, WarGrey::SCADA::Planetlet, int> {
	public:
		virtual ~Diglet() noexcept;

		Diglet(Platform::String^ file_bmp, float view_width, float view_height, double scale = 0.01,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "dig");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	protected:
		void on_appdata(Windows::Foundation::Uri^ ms_appdata_dig, WarGrey::SCADA::DigVectorMap^ doc_dig, int hint) override;
		void on_appdata_not_found(Windows::Foundation::Uri^ ms_appdata_dig, int hint) override {}

	private:
		WarGrey::SCADA::DigVectorMap^ graph_dig;

	private:
		Windows::Foundation::Uri^ ms_appdata_dig;
		double map_x;
		double map_y;
		double map_width;
		double map_height;

	private:
		float view_width;
		float view_height;
		double origin_scale;
	};
}
