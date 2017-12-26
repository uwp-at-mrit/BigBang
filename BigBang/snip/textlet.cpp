#include <cwchar>
#include <cstdarg>
#include <cstdlib>

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "textlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Labellet::Labellet(const wchar_t *fmt, ...) {
    static const int DEFAULT_POOL_SIZE = 1024;
    static wchar_t wpool[DEFAULT_POOL_SIZE];

    int bigSize = DEFAULT_POOL_SIZE - 1;
    wchar_t *pool;
    va_list argl;

    do {
        pool = (bigSize < DEFAULT_POOL_SIZE) ? wpool : (new wchar_t[bigSize + 1]);
        va_start(argl, fmt);
        int status = vswprintf(pool, bigSize + 1, fmt, argl);
        va_end(argl);
        if (status == -1) {
            bigSize = bigSize * 2 + 1;
            if (pool != wpool) delete[] pool;
            pool = nullptr;
        }
    } while (pool == nullptr);

    this->change_text(ref new Platform::String(pool));
    if (pool != wpool) delete[] pool;
}

Labellet::Labellet(Platform::String^ content) {
    this->change_text(content);
}

void Labellet::change_text(Platform::String^ content) {
    this->content = content;
    if (this->label_font == nullptr) {
        this->label_font = make_text_format();
    }
}

void Labellet::fill_extent(float x, float y, float* w, float* h) {
    TextExtent ts = get_text_extent(content, label_font);

    SET_VALUES(w, ts.width, h, ts.height);
};

void Labellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    ds->DrawText(content, x, y, Colors::Snow, label_font);
}

/*************************************************************************************************/
Scalelet::Scalelet(Platform::String^ label, Platform::String^ unit, Platform::String^ subscript, Color& lcolor, Color& scolor) {
	Platform::String^ symbol = speak(label);
	Platform::String^ suffix = " = ";

	this->scale_font = make_text_format("Cambria Math");
	this->unit = make_text_layout(speak(unit), this->scale_font);

	if (subscript == nullptr) {
		this->label = make_text_layout(symbol + suffix, this->scale_font);
	} else {
		Platform::String^ subsymbol = speak(subscript);
		float subsize = this->scale_font->FontSize * 0.618F;
		unsigned int symcount = symbol->Length();
		unsigned int subcount = subsymbol->Length();

		this->label = make_text_layout(symbol + subsymbol + suffix, this->scale_font);
		this->label->SetFontSize(symcount, subcount, subsize);
		this->label->SetFontStyle(symcount, subcount, FontStyle::Italic);
	}

	this->label_color = make_solid_brush(lcolor);
	this->scale_color = make_solid_brush(scolor);
	this->change_scale(0.0F);
}

void Scalelet::change_scale(float value) {
	this->scale = make_text_layout(" " + value.ToString(), this->scale_font);
}

void Scalelet::fill_extent(float x, float y, float* w, float* h) {
	float width = this->label->LayoutBounds.Width + this->scale->LayoutBounds.Width + this->unit->LayoutBounds.Width;
	float height = this->label->LayoutBounds.Height;

	SET_VALUES(w, width, h, height);
};

void Scalelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float xoff = x + this->label->LayoutBounds.Width;
	float swidth = this->scale->LayoutBounds.Width;

	ds->DrawTextLayout(this->label, x, y, this->label_color);
	ds->DrawTextLayout(this->scale, xoff, y, this->scale_color);
	ds->DrawTextLayout(this->unit, xoff + swidth, y, this->label_color);
}
