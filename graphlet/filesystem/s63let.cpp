#include "graphlet/filesystem/s63let.hpp"

#include "datum/flonum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"
#include "datum/time.hpp"

#include "crypto/enckey.hpp"

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
S63let::S63let(Platform::String^ enc, uint64 hw_id, float view_width, float view_height, ICanvasBrush^ background, Platform::String^ rootdir)
	: Planetlet(new S63Frame(enc), GraphletAnchor::LT, background), view_size(Size(view_width, view_height)) /*, map(nullptr) */
	, HW_ID(enc_natural(hw_id)), pseudo_now(0) {
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

	if ((permit->encs.size() + permit->ecss.size()) > 0) {
		this->get_logger()->log_message(Log::Debug, L"%s VERSION %u: (%08u, %02u:%02u:%02u) ENC %d",
			permit->content.ToString()->Data(), permit->version,
			permit->cdate, permit->chour, permit->cminute, permit->csecond,
			permit->encs.size());

		for (size_t idx = 0; idx < permit->encs.size(); idx++) {
			ENCell* cell = &permit->encs[idx];

			if (!cell->malformed()) {
				Natural plainsum = enc_cell_permit_checksum(cell->name, strlen(cell->name),
					cell->expiry_year, cell->expiry_month, cell->expiry_day,
					cell->ECK1, cell->ECK2);

				if (enc_cell_permit_encrypt(this->HW_ID, plainsum) == cell->checksum) {
					long long now = ((this->pseudo_now <= 0) ? current_seconds() : this->pseudo_now);
					long long expiry_date = make_seconds(cell->expiry_year, cell->expiry_month, cell->expiry_day);

					if (expiry_date < now) {
						this->get_logger()->log_message(Log::Warning, enc_speak(ENCErrorCode::SSE15, cell->name));
					} else if (seconds_add_days(expiry_date, -30) < now) {
						this->get_logger()->log_message(Log::Warning, enc_speak(ENCErrorCode::SSE20, cell->name));
					}

					cell->key1 = enc_cell_permit_decrypt(this->HW_ID, cell->ECK1);
					cell->key1 = enc_cell_permit_decrypt(this->HW_ID, cell->ECK2);

					this->get_logger()->log_message(Log::Debug, L"%d[%S]: %S[%s]: %S%S%S before %d-%02d-%02d",
						idx, cell->data_server_id.c_str(),
						cell->name, cell->type.ToString()->Data(),
						cell->ECK1.to_hexstring().c_str(), cell->ECK2.to_hexstring().c_str(), cell->checksum.to_hexstring().c_str(),
						cell->expiry_year, cell->expiry_month, cell->expiry_day);
				} else {
					this->get_logger()->log_message(Log::Error, enc_speak(ENCErrorCode::SSE13, cell->name));
					cell->checksum = 0U;
				}
			} else {
				this->get_logger()->log_message(Log::Error, enc_speak(ENCErrorCode::SSE12, cell->name));
			}
		}
	} else {
		this->get_logger()->log_message(Log::Warning, enc_speak(ENCErrorCode::SSE11));
	}

	this->PERMIT_TXT = permit;
}

void S63let::on_public_key(Platform::String^ ms_appdata, ENChartDocument^ doc) {
	this->IHO_PUB = static_cast<PublicKeyDoc^>(doc);

	this->get_logger()->log_message(Log::Info, L"p: %S", this->IHO_PUB->p.to_hexstring().c_str());
	this->get_logger()->log_message(Log::Info, L"q: %S", this->IHO_PUB->q.to_hexstring().c_str());
	this->get_logger()->log_message(Log::Info, L"g: %S", this->IHO_PUB->g.to_hexstring().c_str());
	this->get_logger()->log_message(Log::Info, L"y: %S", this->IHO_PUB->y.to_hexstring().c_str());
}

void S63let::on_certificate(Platform::String^ ms_appdata, ENChartDocument^ doc) {
	this->IHO_CRT = static_cast<CertificateDoc^>(doc);
}

bool S63let::ready() {
	return (this->PERMIT_TXT != nullptr);
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
void S63let::set_pseudo_date(long long year, long long month, long long day) {
	this->pseudo_now = make_seconds(year, month, day);
}

ENChartDoctype S63let::filter_file(Platform::String^ filename, Platform::String^ _ext) {
	ENChartDoctype ft = ENChartDoctype::_;

	if (filename->Equals("PERMIT.TXT")) {
		ft = ENChartDoctype::PERMIT;
	} else if (_ext->Equals(".PUB")) {
		ft = ENChartDoctype::PublicKey;
	} else if (_ext->Equals(".CRT")) {
		ft = ENChartDoctype::Certificate;
	}

	return ft;
}

void S63let::on_appdata(Platform::String^ ms_appdata, ENChartDocument^ doc, ENChartDoctype type) {
	switch (type) {
	case ENChartDoctype::PERMIT: this->on_permit(ms_appdata, doc); break;
	case ENChartDoctype::PublicKey: this->on_public_key(ms_appdata, doc); break;
	case ENChartDoctype::Certificate: this->on_certificate(ms_appdata, doc); break;
	}
}
