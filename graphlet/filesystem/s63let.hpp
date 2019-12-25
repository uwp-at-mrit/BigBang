#pragma once

#include <deque>

#include "graphlet/planetlet.hpp"

#include "graphlet/filesystem/msappdataloguelet.hxx"

#include "graphlet/filesystem/enchart/reader/enctype.hxx"
#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"
#include "graphlet/filesystem/enchart/reader/pubdoc.hxx"

#include "datum/natural.hpp"

namespace WarGrey::SCADA {
	private class S63let : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::ENChartDocument, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ENChartDoctype> {
	public:
		S63let(Platform::String^ enc, uint64 HW_ID, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "s63");

	public:
		void translate(float deltaX, float deltaY);
		void zoom(float zx, float zy, float deltaScale);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	public:
		void set_pseudo_date(long long year, long long month, long long day);

	protected:
		WarGrey::SCADA::ENChartDoctype filter_file(Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::SCADA::ENChartDocument^ doc_dig, WarGrey::SCADA::ENChartDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, ENChartDoctype type) override {}

	private:
		void on_permit(Platform::String^ ms_appdata, WarGrey::SCADA::ENChartDocument^ doc_dig);
		void on_public_key(Platform::String^ ms_appdata, WarGrey::SCADA::ENChartDocument^ doc_dig);

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::PermitDoc^ PERMIT_TXT;
		WarGrey::SCADA::PublicKeyDoc^ IHO_PUB;

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		//WarGrey::SCADA::DigMaplet* map;
		std::deque<Platform::Object^> icons;

	private:
		Windows::Foundation::Size view_size;

	private:
		WarGrey::SCADA::Natural HW_ID;
		long long pseudo_now;
	};
}
