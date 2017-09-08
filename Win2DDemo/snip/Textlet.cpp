#include <cwchar>
#include <cstdarg>
#include <cstdlib>

#include "textlet.hpp"
#include "canvas.hxx"

using namespace WarGrey::Win2DDemo;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Media;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

const int DEFAULT_POOL_SIZE = 1024;
static wchar_t wpool[DEFAULT_POOL_SIZE];

Textlet::~Textlet() {}

Textlet::Textlet(const wchar_t *fmt, ...) {
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

Textlet::Textlet(Platform::String^ content) {
    this->change_text(content);
}

SnipTypes Textlet::get_type() {
    return SnipTypes::Text;
}

void Textlet::change_text(Platform::String^ content) {
    this->content = content;
    if (this->layout_config == nullptr) {
        this->layout_config = ref new CanvasTextFormat();
        this->layout_config->WordWrapping = CanvasWordWrapping::NoWrap;
    }
}

void Textlet::fill_extent(float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    TextExtent ts = get_text_extent(content, layout_config);

    if (width != nullptr) (*width) = ts.width;
    if (height != nullptr) (*height) = ts.height;
    if (descent != nullptr) (*descent) = ts.descent;
    if (space != nullptr) (*space) = ts.space;
    if (lspace != nullptr) (*lspace) = ts.lspace;
    if (rspace != nullptr) (*rspace) = ts.rspace;
};

void Textlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    ds->DrawText(content, x, y, Colors::Black, layout_config);
}

/*************************************************************************************************/
SnipIcon* WarGrey::Win2DDemo::make_textlet_icon(float size, Windows::UI::Color color) {
    return new TextIcon(size, color);
}

TextIcon::TextIcon(float size, Windows::UI::Color color) : SnipIcon(size) {
    this->foreground = color;
}

TextIcon::~TextIcon() {}

void TextIcon::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    ds->FillRectangle(x, y, size, size, this->foreground);
}

Snip* TextIcon::create_snip() {
    return new Textlet("text snip");
}
