#include "graphlet/filesystem/s63let.hpp"

#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "math.hpp"
#include "planet.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private class S63Frame : public Planet {
	public:
		virtual ~S63Frame() noexcept {}

		S63Frame(Platform::String^ name) : Planet(name) {}

	public:
		bool can_select(IGraphlet* g) override {
			return false;
		}
	};

	private ref struct S63IconEntity sealed {
	internal:
		S63IconEntity(IGraphlet* icon, double x, double y) : icon(icon), x(x), y(y) {}

	internal:
		IGraphlet* icon;
		double x;
		double y;
	};
}

/*************************************************************************************************/
S63let::S63let(Platform::String^ enc, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new S63Frame(enc), GraphletAnchor::LT, background), view_size(Size(view_width, view_height)) /*, map(nullptr) */ {
	this->ms_appdata_rootdir = ((rootdir == nullptr) ? enc : rootdir + "\\" + enc);
	this->enable_stretch(false, false);
	this->enable_events(true, false);
	this->disable_wheel_translation(true);
}

void S63let::construct() {
	Planetlet::construct();

	this->font = make_bold_text_format(32.0F);
	this->cd(this->ms_appdata_rootdir);
}

void S63let::on_permit(Platform::String^ ms_appdata, ENChartDocument^ doc) {
	PermitDoc^ permit = static_cast<PermitDoc^>(doc);

	if ((permit->encs.size() + permit->ecss.size()) == 0) {
		this->get_logger()->log_message(Log::Warning, enc_speak(ENCErrorCode::SSE11));
	} else {
		this->get_logger()->log_message(Log::Info, L"%s VERSION %u: (%08u, %02u:%02u:%02u) ENC %d",
			permit->content.ToString()->Data(), permit->version,
			permit->cdate, permit->chour, permit->cminute, permit->csecond,
			permit->encs.size());

		for (size_t idx = 0; idx < permit->encs.size(); idx++) {
			this->get_logger()->log_message(Log::Info, L"%d: %S %s[%s]", idx,
				permit->encs[idx].permit.c_str(), permit->encs[idx].type.ToString()->Data(),
				permit->encs[idx].comment->Data());
		}
	}
}

bool S63let::ready() {
	return (this->graph_dig != nullptr);
}

void S63let::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->view_size.Width, h, this->view_size.Height);
}

void S63let::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float border_thickness = 2.0F;
	float offset = border_thickness * 0.5F;

	Planetlet::draw(ds, x, y, Width, Height);

	ds->DrawRectangle(x + offset, y + offset, Width - border_thickness, Height - border_thickness,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		border_thickness);
}

void S63let::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TextExtent te;
	CanvasGeometry^ pname = paragraph(file_name_from_path(this->ms_appdata_rootdir), this->font, &te);
	float px = x + (Width - te.width) * 0.5F;
	float py = y + (Height - te.height) * 0.5F;

	this->draw(ds, x, y, Width, Height);
	ds->FillGeometry(pname, px, py, Colours::GrayText);
}

void S63let::translate(float deltaX, float deltaY) {
	//if (this->map != nullptr) {
	//	this->planet->begin_update_sequence();
	//	this->map->translate(deltaX, deltaY);
	//	this->relocate_icons();
	//	this->planet->end_update_sequence();
	//}
}

void S63let::zoom(float zx, float zy, float deltaScale) {
	//if (this->map != nullptr) {
	//	this->planet->begin_update_sequence();
	//	this->map->zoom(zx, zy, deltaScale);
	//	this->relocate_icons();
	//	this->planet->end_update_sequence();
	//}
}

void S63let::relocate_icons() {
	//for (auto it = this->icons.begin(); it != this->icons.end(); it++) {
	//	S63IconEntity^ ent = static_cast<S63IconEntity^>(*it);
	//	float2 ipos = this->map->position_to_local(ent->x, ent->y);

	//	this->planet->move_to(ent->icon, ipos.x, ipos.y, GraphletAnchor::CC);
	//}
}

/*************************************************************************************************/
ENChartDoctype S63let::filter_file(Platform::String^ filename, Platform::String^ _ext) {
	ENChartDoctype ft = ENChartDoctype::_;

	if (filename->Equals("PERMIT.TXT")) {
		ft = ENChartDoctype::PERMIT;
	}

	return ft;
}

void S63let::on_appdata(Platform::String^ ms_appdata, ENChartDocument^ doc, ENChartDoctype type) {
	switch (type) {
	case ENChartDoctype::PERMIT: this->on_permit(ms_appdata, doc); break;
	}
}
