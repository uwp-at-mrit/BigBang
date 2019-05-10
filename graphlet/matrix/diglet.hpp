#pragma once

#include "graphlet/msappdatalet.hxx"
#include "graphlet/planetlet.hpp"

namespace WarGrey::SCADA {
	private ref class DigVectorMap {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::DigVectorMap^>^ LoadAsync(Platform::String^ dig);
	};

	private class Diglet : public virtual WarGrey::SCADA::IMsAppdatalet<WarGrey::SCADA::DigVectorMap, WarGrey::SCADA::Planetlet, int> {
	public:
		virtual ~Diglet() noexcept;

		Diglet(Platform::String^ file_bmp,
			WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::CC,
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
		Windows::Foundation::Rect window;
		Windows::Foundation::Uri^ ms_appdata_dig;
	};
}
