#include <cwchar>
#include <cstdarg>
#include <cstdlib>

#include "textlet.hpp"
#include "canvas.hxx"

using namespace Win2D::Snip;
using namespace Win2D::UIElement;

using namespace std;
using namespace Platform;
using namespace Windows::UI;
using namespace Windows::UI::Xaml::Media;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

#define DEFAULT_POOL_SIZE 1024
static wchar_t wpool[DEFAULT_POOL_SIZE];

static void textlet_set_large_string(Textlet* self, size_t size, const wchar_t* fmt, va_list argl) {
    wchar_t* largePool = (wchar_t*)calloc(size + 1, sizeof(wchar_t));

    vswprintf(largePool, size + 1, fmt, argl);
    va_end(argl);
    self->change_text("(" + size.ToString() + ")");
    free(largePool);
}

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

    this->change_text(ref new String(pool));
    if (pool != wpool) delete[] pool;
}

Textlet::Textlet(String^ content) {
    this->change_text(content);
}

SnipTypes Textlet::get_type() {
    return SnipTypes::Text;
}

void Textlet::change_text(String^ content) {
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
    ds->DrawText(content, (float)x, (float)y, Colors::Black, layout_config);
}

/*************************************************************************************************/
SnipIcon* Win2D::Snip::make_textlet_icon(float size, unsigned char r, unsigned char g, unsigned char b) {
    return new TextIcon(size, r, g, b);
}

TextIcon::TextIcon(float size, unsigned char r, unsigned char g, unsigned char b) : SnipIcon(size) {
    this->foreground = { 255, r, g, b };
}

void TextIcon::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    ds->FillRectangle(x, y, size, size, this->foreground);
}
