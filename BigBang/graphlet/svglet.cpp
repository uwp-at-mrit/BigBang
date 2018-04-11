#include <ppltasks.h>

#include "graphlet/svglet.hpp"

#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Svg;

/*************************************************************************************************/
Svglet::Svglet(Platform::String^ file_svg, float width, float height, Platform::String^ root) {
	this->viewport.Width = width;
	this->viewport.Height = height;
	this->file_svg = make_text_layout(file_svg, make_text_format());

	{ // load svg asynchronously
		CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();
		FileAccessMode FAM = FileAccessMode::Read;
		StorageOpenOptions SOO = StorageOpenOptions::AllowOnlyReaders;
		FileOpenDisposition FOD = FileOpenDisposition::OpenExisting;
		Platform::String^ ms_appx_svg = "ms-appx:///" + ((root == nullptr) ? file_svg : (root + "/" + file_svg));

		syslog(Log::Info, Windows::ApplicationModel::Package::Current->InstalledLocation->Path->ToString());
		
		create_task(FileRandomAccessStream::OpenAsync(ms_appx_svg, FAM, SOO, FOD)).then([=](task<IRandomAccessStream^> svg) {
			return CanvasSvgDocument::LoadAsync(shared_ds, svg.get());
		}).then([=](task<CanvasSvgDocument^> doc_svg) {
			try {
				this->graph_svg = doc_svg.get();
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Error, L"Failed to load %s: %s", ms_appx_svg->Data(), e->Message->Data());
			} catch (task_canceled&) {
				this->get_logger()->log_message(Log::Debug, L"Loading %s is canceled.", ms_appx_svg->Data());
			}
		});
	}
}

void Svglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->viewport.Width, h, this->viewport.Height);
}

void Svglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->graph_svg != nullptr) {
		ds->DrawSvg(this->graph_svg, this->viewport, x, y);
	} else {
		this->draw_on_error(ds, x, y, Width, Height);
	}
}

void Svglet::draw_on_error(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	auto color = Colours::GrayText;
	float thickness = 2.0F;
	float x0 = x + thickness * 0.5F;
	float y0 = y + thickness * 0.5F;
	float xn = x0 + this->viewport.Width - thickness;
	float yn = y0 + this->viewport.Height - thickness;

	ds->DrawRectangle(x0, y0, xn - x0, yn - y0, color, thickness);
	ds->DrawLine(x0, y0, xn, yn, color, thickness);
	ds->DrawLine(x0, yn, xn, y0, color, thickness);
	ds->DrawTextLayout(this->file_svg, x + thickness, y, color);
}
