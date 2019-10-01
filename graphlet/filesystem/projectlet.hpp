#pragma once

#include <deque>
#include <map>

#include "graphlet/filesystem/msappdatalet.hxx"
#include "graphlet/filesystem/msappdataloguelet.hxx"
#include "graphlet/planetlet.hpp"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private enum class ProjectFileType { DIG, _ };

	private ref class DigMap sealed {
	public:
		static Windows::Foundation::IAsyncOperation<WarGrey::SCADA::DigMap^>^ load_async(Platform::String^ dig, WarGrey::SCADA::ProjectFileType type);

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

	private class Projectlet : public virtual WarGrey::SCADA::IMsAppdataLoguelet<WarGrey::SCADA::DigMap, WarGrey::SCADA::Planetlet, WarGrey::SCADA::ProjectFileType> {
	public:
		virtual ~Projectlet() noexcept;

		Projectlet(Platform::String^ project, float view_width, float view_height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background = nullptr,
			Platform::String^ rootdir = "projects");

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
		WarGrey::SCADA::ProjectFileType filter_file(Platform::String^ file, Platform::String^ _ext) override;
		void on_appdata(Platform::String^ file, WarGrey::SCADA::DigMap^ doc_dig, WarGrey::SCADA::ProjectFileType type) override;
		void on_appdata_not_found(Platform::String^ file, ProjectFileType type) override {}

	private:
		void relocate_icons();

	private:
		WarGrey::SCADA::DigMap^ graph_dig;

	private:
		Platform::String^ ms_appdata_rootdir;

	private:
		WarGrey::SCADA::IGraphlet* map;
		std::deque<Platform::Object^> icons;
		float view_width;
		float view_height;
	};
}
