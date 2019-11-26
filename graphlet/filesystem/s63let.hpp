#pragma once

#include <deque>

#include "graphlet/planetlet.hpp"
#include "graphlet/vessellet.hpp"

#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/filesystem/configuration/colorplotlet.hpp"
#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/digmaplet.hpp"
#include "graphlet/filesystem/project/xyzlet.hpp"
#include "graphlet/filesystem/project/tracelinelet.hpp"
#include "graphlet/filesystem/project/sectionlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private class S63let : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::ProjectDocument, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ProjectDoctype> {
	public:
		S63let(Platform::String^ enc, float view_width, float view_height,
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

	protected:
		WarGrey::SCADA::ProjectDoctype filter_file(Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig, WarGrey::SCADA::ProjectDoctype type) override;
		void on_appdata_not_found(Platform::String^ ms_appdata, ProjectDoctype type) override {}

	private:
		void on_dig(Platform::String^ ms_appdata, WarGrey::SCADA::ProjectDocument^ doc_dig);

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::ProjectDocument^ graph_dig;

	private:
		Platform::String^ ms_appdata_rootdir;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

	private: // graphlets are managed by the Planetlet
		WarGrey::SCADA::DigMaplet* map;
		std::deque<Platform::Object^> icons;

	private:
		Windows::Foundation::Size view_size;
	};
}
