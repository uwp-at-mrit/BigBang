#include "graphlet/symbol/heaterlet.hpp"

#include "shape.hpp"
#include "polar.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static float default_thickness = 2.0F;
static double default_alpha_degrees = 36.0;
static unsigned int dynamic_gradient_step = 8U;

/*************************************************************************************************/
Heaterlet::Heaterlet(float radius, double degrees)
	: Heaterlet(HeaterStatus::Default, radius, degrees) {}

Heaterlet::Heaterlet(HeaterStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, radius, degrees) {
	this->radiusY = this->radiusX * 0.618F;
	this->brdiff = default_thickness * 2.5F;
}

void Heaterlet::construct() {
	auto thread_builder = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	auto body_box = polar_rectangle(this->radiusX - this->brdiff, this->radiusY - this->brdiff, default_alpha_degrees, 0.0);

	{ // make thread path
		Rect box = body_box->ComputeBounds();
		unsigned int thread_half_step = 4U;
		float thread_half_width = box.Width * 0.5F - default_thickness;
		float thread_half_height = box.Height * 0.5F - default_thickness;
		float thread_interval = thread_half_width / float(thread_half_step + 1);

		thread_builder->BeginFigure(-thread_half_width, 0.0F);

		for (unsigned int i = 0; i <= (thread_half_step * 2U); i++) {
			float x = -thread_half_width + thread_interval * float(i + 1);
			float y = 0.0F;
			
			switch (i % 4) {
			case 0: y = -thread_half_height; break;
			case 2: y = +thread_half_height; break;
			}

			thread_builder->AddLine(x, y);
		}

		thread_builder->AddLine(thread_half_width, 0.0F);
		thread_builder->EndFigure(CanvasFigureLoop::Open);
	}

	this->body = geometry_rotate(body_box, this->degrees);
	this->border = geometry_rotate(polar_rectangle(this->radiusX, this->radiusY, default_alpha_degrees, 0.0), this->degrees);
	this->thread = geometry_draft(geometry_rotate(CanvasGeometry::CreatePath(thread_builder), this->degrees),
		default_thickness, make_round_stroke_style());
}

void Heaterlet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	auto box = this->border->ComputeStrokeBounds(default_thickness);
	float hspace = this->width - box.Width;
	float vspace = this->height - box.Height;

	SET_BOXES(top, bottom, vspace * 0.5F);
	SET_BOXES(left, right, hspace * 0.5F);
}

void Heaterlet::update(long long count, long long interval, long long uptime) {
	double alpha_raw = double(count % dynamic_gradient_step) / double(dynamic_gradient_step - 1);
	
	switch (this->get_status()) {
	case HeaterStatus::Starting: {
		this->thread_color = Colours::make(this->get_style().thread_color, alpha_raw);
		this->notify_updated();
	} break;
	case HeaterStatus::Stopping: {
		this->thread_color = Colours::make(this->get_style().thread_color, 1.0 - alpha_raw);
		this->notify_updated();
	} break;
	}
}

void Heaterlet::prepare_style(HeaterStatus state, HeaterStyle& s) {
	switch (state) {
	case HeaterStatus::Broken: {
		CAS_SLOT(s.body_color, Colours::Red);
	} break;
	case HeaterStatus::Starting: case HeaterStatus::Stopping: {
		CAS_SLOT(s.thread_color, Colours::DarkOrange);
	}; break;
	case HeaterStatus::Running: {
		CAS_SLOT(s.thread_color, Colours::DarkOrange);
		CAS_SLOT(s.body_color, Colours::MistyRose);
	}; break;
	case HeaterStatus::Stopped: {
		CAS_SLOT(s.body_color, Colours::DimGray);
	} break;
	case HeaterStatus::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HeaterStatus::Unstartable: {
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HeaterStatus::Unstoppable: {
		CAS_SLOT(s.skeleton_color, Colours::Red);
		CAS_SLOT(s.thread_color, Colours::Orange);
	}; break;
	case HeaterStatus::Auto: {
		CAS_SLOT(s.thread_color, Colours::AliceBlue);
		CAS_SLOT(s.body_color, Colours::SkyBlue);
	}; break;
	}

	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.border_color, Colours::Silver);
	CAS_SLOT(s.thread_color, Colours::Gray);
	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.skeleton_color, s.body_color);

	// NOTE: The others can be nullptr;
}

void Heaterlet::on_status_changed(HeaterStatus state) {
	this->thread_color = nullptr;
}

void Heaterlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const HeaterStyle style = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->FillGeometry(this->border, cx, cy, Colours::Background);
	ds->FillGeometry(this->body, cx, cy, style.body_color);
	ds->DrawGeometry(this->body, cx, cy, style.skeleton_color, default_thickness);
	
	ds->DrawCachedGeometry(this->thread, cx, cy,
		(this->thread_color != nullptr) ? this->thread_color : style.thread_color);

	ds->DrawGeometry(this->border, cx, cy,
		(this->remote_control ? style.remote_color : style.border_color),
		default_thickness);
}

void Heaterlet::set_remote_control(bool on) {
	this->remote_control = on;
}
