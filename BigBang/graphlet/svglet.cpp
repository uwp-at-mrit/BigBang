#include <ppltasks.h>

#include "graphlet/svglet.hpp"

#include "string.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Svg;

/*************************************************************************************************/
Svglet::Svglet(Platform::String^ file, Platform::String^ rootdir) : Svglet(file, 0.0F, 0.0F, rootdir) {}

Svglet::Svglet(Platform::String^ file, float width, float height, Platform::String^ rootdir) {
	this->viewport.Width = width;
	this->viewport.Height = height;

	{ // adjust file name
		Platform::String^ file_svg = (file_extension_from_path(file) == nullptr) ? (file + ".svg") : file;
		Platform::String^ path_svg = ((rootdir == nullptr) ? file_svg : (rootdir + "/" + file_svg));

		this->ms_appx_svg = ref new Uri("ms-appx:///usr/share/" + path_svg);
	}
}

Svglet::~Svglet() {

}

void Svglet::construct() {
	create_task(StorageFile::GetFileFromApplicationUriAsync(this->ms_appx_svg)).then([=](task<StorageFile^> svg) {
		this->get_logger()->log_message(Log::Debug,
			L"Found the graphlet source: %s",
			this->ms_appx_svg->ToString()->Data());

		return svg.get()->OpenAsync(FileAccessMode::Read, StorageOpenOptions::AllowOnlyReaders);
	}).then([=](task<IRandomAccessStream^> svg) {
		return CanvasSvgDocument::LoadAsync(CanvasDevice::GetSharedDevice(), svg.get());
	}).then([=](task<CanvasSvgDocument^> doc_svg) {
		try {
			this->graph_svg = doc_svg.get();
			this->root = this->graph_svg->Root;

			if (this->viewport.Width != 0.0F) {
				root->SetLengthAttribute("width", 100.0F, CanvasSvgLengthUnits::Percentage);
			}

			if (this->viewport.Height != 0.0F) {
				root->SetLengthAttribute("height", 100.0F, CanvasSvgLengthUnits::Percentage);
			}
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Error,
				L"Failed to load %s: %s",
				this->ms_appx_svg->ToString()->Data(),
				e->Message->Data());
		} catch (task_canceled&) {
			this->get_logger()->log_message(Log::Debug,
				L"Loading %s is canceled.",
				this->ms_appx_svg->ToString()->Data());
		}
	});
}

void Svglet::fill_extent(float x, float y, float* w, float* h) {
	if (this->root != nullptr) {
		CanvasSvgLengthUnits units;

		if (this->viewport.Width == 0.0F) {
			this->viewport.Width = this->get_length_attribute("width", &units, false);
		}

		if (this->viewport.Height == 0.0F) {
			this->viewport.Height = this->get_length_attribute("height", &units, false);
		}
	}

	SET_VALUES(w, this->viewport.Width, h, this->viewport.Height);
}

float Svglet::get_length_attribute(Platform::String^ name, CanvasSvgLengthUnits* units, bool inherited) {
	// NOTE: this value is also the default length if both the client program and the SVG itself do not specific the size.
	float value = 100.0F;
	(*units) = CanvasSvgLengthUnits::Percentage;

	if (this->root != nullptr) {
		if (this->root->IsAttributeSpecified(name, inherited)) {
			value = this->root->GetLengthAttribute(name, units);
		}
	}

	return value;
}

void Svglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->graph_svg != nullptr) {
		ds->DrawSvg(this->graph_svg, this->viewport, x, y);
	} else {
		this->draw_on_error(ds, x, y, Width, Height);
	}
}

void Svglet::draw_on_error(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	static auto font = make_text_format();
	auto color = Colours::GrayText;
	float thickness = 2.0F;
	float x0 = x + thickness * 0.5F;
	float y0 = y + thickness * 0.5F;
	float xn = x0 + this->viewport.Width - thickness;
	float yn = y0 + this->viewport.Height - thickness;

	ds->DrawRectangle(x0, y0, xn - x0, yn - y0, color, thickness);
	ds->DrawLine(x0, y0, xn, yn, color, thickness);
	ds->DrawLine(x0, yn, xn, y0, color, thickness);
	ds->DrawText(file_name_from_path(this->ms_appx_svg), x + thickness, y, color, font);
}
