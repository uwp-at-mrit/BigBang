#pragma once

#include <deque>

#include "graphlet/planetlet.hpp"

#include "graphlet/filesystem/msappdataloguelet.hxx"

#include "graphlet/filesystem/enchart/reader/enctype.hxx"
#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"
#include "graphlet/filesystem/enchart/reader/pubdoc.hxx"
#include "graphlet/filesystem/enchart/reader/crtdoc.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private class S63let : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::DTPM::ENChartDocument, WarGrey::SCADA::Planetlet, WarGrey::DTPM::ENChartDoctype> {
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
		bool filter_folder(Platform::String^ parent, Platform::String^ dirname) override;
		WarGrey::DTPM::ENChartDoctype filter_file(Platform::String^ parent, Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::DTPM::ENChartDocument^ doc_enc, WarGrey::DTPM::ENChartDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, ENChartDoctype type) override {}

	private:
		void on_permit(Platform::String^ ms_appdata, WarGrey::DTPM::ENChartDocument^ doc_enc);
		void on_public_key(Platform::String^ ms_appdata, WarGrey::DTPM::ENChartDocument^ doc_enc);
		void on_certificate(Platform::String^ ms_appdata, WarGrey::DTPM::ENChartDocument^ doc_enc);

	private:
		void relocate_icons();

	private:
		WarGrey::DTPM::PermitDoc^ PERMIT_TXT;
		WarGrey::DTPM::PublicKeyDoc^ IHO_PUB;
		WarGrey::DTPM::CertificateDoc^ IHO_CRT;

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		//WarGrey::SCADA::DigMaplet* map;
		std::deque<Platform::Object^> icons;

	private:
		Windows::Foundation::Size view_size;

	private:
		WarGrey::DTPM::Natural HW_ID;
		long long pseudo_now;
	};
}
