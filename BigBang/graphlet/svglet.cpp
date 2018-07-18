#include "graphlet/svglet.hpp"
#include "planet.hpp"

#include "colorspace.hpp"
#include "path.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Svg;

static float svg_length_attribute(CanvasSvgNamedElement^ target, Platform::String^ name, bool* relative, float default_value, bool inherited) {
	CanvasSvgLengthUnits units = CanvasSvgLengthUnits::Percentage;
	float value = default_value;

	if (target->IsAttributeSpecified(name, inherited)) {
		value = target->GetLengthAttribute(name, &units);
	}

	SET_BOX(relative, units == CanvasSvgLengthUnits::Percentage);

	return value;
}

/*************************************************************************************************/
ISvglet::ISvglet(float width, float height) {
	this->viewport.Width = width;
	this->viewport.Height = height;
}

ISvglet::~ISvglet() {
	if (this->ms_appx_svg != nullptr) {
		this->unload(this->ms_appx_svg);
	}
}

void ISvglet::construct() {
	this->ms_appx_svg = ms_appx_file(this->name(), ".svg", this->rootdir());
	this->load(this->ms_appx_svg, 0);
}

void ISvglet::on_appx(Uri^ ms_appx_svg, CanvasSvgDocument^ doc_svg, int hint) {
	this->graph_svg = doc_svg;
	this->root = this->graph_svg->Root;

	if ((this->viewport.Width <= 0.0F) || (this->viewport.Height <= 0.0F)) {
		bool relative_width, relative_height;		
		float width = this->get_length_attribute("width", &relative_width, 100.0F, false);
		float height = this->get_length_attribute("height", &relative_height, 100.0F, false);

		// TODO: what if one of the lengths is measured in percentage
		if (this->viewport.Width <= 0.0F) {
			if (this->viewport.Height <= 0.0F) {
				this->viewport.Height = height;
			}
			this->viewport.Width = width * (this->viewport.Height / height);
		} else {
			this->viewport.Height = height * (this->viewport.Width / width);
		}
	}

	this->set_percentage_attribute("width", 100.0F);
	this->set_percentage_attribute("height", 100.0F);

	this->on_ready();
}

bool ISvglet::ready() {
	return (this->graph_svg != nullptr);
}

void ISvglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->viewport.Width, h, this->viewport.Height);
}

void ISvglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawSvg(this->graph_svg, this->viewport, x, y);
}

void ISvglet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appx_svg);

	draw_invalid_bitmap(hint, ds, x, y, this->viewport.Width, this->viewport.Height);
}

/*************************************************************************************************/
void ISvglet::set_shape_color(Platform::String^ id, Windows::UI::Color& c) {
	this->set_fill_color(id, c);
	this->set_stroke_color(id, c);
}

void ISvglet::set_shape_color(Platform::String^ id, unsigned int hex, double alpha) {
	this->set_shape_color(id, rgba(hex, alpha));
}

void ISvglet::set_shape_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush) {
	this->set_shape_color(id, brush->Color);
}

Color ISvglet::get_fill_color(Platform::String^ id, Color& default_color) {
	return this->get_child_color_attribute(id, "fill", default_color);
}

void ISvglet::set_fill_color(Platform::String^ id, Windows::UI::Color& c) {
	this->set_child_color_attribute(id, "fill", c);
}

void ISvglet::set_fill_color(Platform::String^ id, unsigned int hex, double alpha) {
	this->set_fill_color(id, rgba(hex, alpha));
}

void ISvglet::set_fill_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush) {
	this->set_fill_color(id, brush->Color);
}

Color ISvglet::get_stroke_color(Platform::String^ id, Color& default_color) {
	return this->get_child_color_attribute(id, "stroke", default_color);
}

void ISvglet::set_stroke_color(Platform::String^ id, Windows::UI::Color& c) {
	this->set_child_color_attribute(id, "stroke", c);
}

void ISvglet::set_stroke_color(Platform::String^ id, unsigned int hex, double alpha) {
	this->set_stroke_color(id, rgba(hex, alpha));
}

void ISvglet::set_stroke_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush) {
	this->set_stroke_color(id, brush->Color);
}

float ISvglet::get_stroke_width(Platform::String^ id, float default_width) {
	return this->get_child_number_attribute(id, "stroke-width", default_width);
}

void ISvglet::set_stroke_width(Platform::String^ id, float width) {
	this->set_child_number_attribute(id, "stroke-width", width);
}

/*************************************************************************************************/
void ISvglet::set_number_attribute(Platform::String^ name, float v) {
	this->root->SetLengthAttribute(name, v, CanvasSvgLengthUnits::Number);
}

void ISvglet::set_percentage_attribute(Platform::String^ name, float v) {
	this->root->SetLengthAttribute(name, v, CanvasSvgLengthUnits::Percentage);
}

float ISvglet::get_length_attribute(Platform::String^ name, bool* relative, float default_value, bool inherited) {
	float value = default_value;

	if (this->root != nullptr) {
		value = svg_length_attribute(this->root, name, relative, default_value, inherited);
	}

	return value;
}

/** WARNING
 * These methods are intended to be invoked correctly, so itself does not check the input.
 * That means the `id` should be defined in the SVG document, or the application will crash in development stage.
 */
float ISvglet::get_child_length_attribute(Platform::String^ id, Platform::String^ attribute, bool* relative, float default_value, bool inherited) {
	CanvasSvgNamedElement^ child = this->graph_svg->FindElementById(id);

	return svg_length_attribute(child, attribute, relative, default_value, inherited);
}

float ISvglet::get_child_number_attribute(Platform::String^ id, Platform::String^ attribute, float default_value, bool inherited) {
	bool who_cares = true;

	return this->get_child_length_attribute(id, attribute, &who_cares, default_value, inherited);
}

void ISvglet::set_child_number_attribute(Platform::String^ id, Platform::String^ attribute, float v) {
	CanvasSvgNamedElement^ child = this->graph_svg->FindElementById(id);
	
	child->SetLengthAttribute(attribute, v, CanvasSvgLengthUnits::Number);
}

void ISvglet::set_child_percentage_attribute(Platform::String^ id, Platform::String^ attribute, float v) {
	CanvasSvgNamedElement^ child = this->graph_svg->FindElementById(id);
	
	child->SetLengthAttribute(attribute, v, CanvasSvgLengthUnits::Percentage);
}

Color ISvglet::get_child_color_attribute(Platform::String^ id, Platform::String^ attribute, Color& default_color, bool inherited) {
	CanvasSvgNamedElement^ child = this->graph_svg->FindElementById(id);

	return child->IsAttributeSpecified(attribute, inherited) ? child->GetColorAttribute(attribute) : default_color;
}

void ISvglet::set_child_color_attribute(Platform::String^ id, Platform::String^ attribute, Windows::UI::Color& c) {
	CanvasSvgNamedElement^ child = this->graph_svg->FindElementById(id);

	child->SetColorAttribute(attribute, c);
}

/*************************************************************************************************/
Svgmaplet::Svgmaplet(Platform::String^ file, Platform::String^ rootdir)
	: Svgmaplet(file, 0.0F, 0.0F, rootdir) {}

Svgmaplet::Svgmaplet(Platform::String^ file, float width, float height, Platform::String^ rootdir)
	: ISvglet(width, height), file_svg(file), stone_subdir(rootdir) {}

Platform::String^ Svgmaplet::name() {
	return this->file_svg;
}

Platform::String^ Svgmaplet::rootdir() {
	return this->stone_subdir;
}
