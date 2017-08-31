#include <cwchar>
#include <cstdarg>
#include <cstdlib>

#include "Textlet.h"
#include "canvas.h"

using namespace Win2D::Sniplet;
using namespace Win2D::UIElement;

using namespace std;
using namespace Platform;
using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

#define DEFAULT_POOL_SIZE 1024
static wchar_t wpool[DEFAULT_POOL_SIZE];

static void TextletSetLargeString(Textlet* self, size_t size, const wchar_t* fmt, va_list argl) {
    wchar_t* largePool = (wchar_t*)calloc(size + 1, sizeof(wchar_t));

    vswprintf(largePool, size + 1, fmt, argl);
    va_end(argl);
    self->SetText("(" + size.ToString() + ")");
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

    this->SetText(ref new String(pool));
    if (pool != wpool) delete[] pool;
}

Textlet::Textlet(String^ content) {
    this->SetText(content);
}

SnipTypes Textlet::GetType() {
    return SnipTypes::Text;
}

void Textlet::SetText(String^ content) {
    this->content = content;
    if (this->font == nullptr) {
        this->font = ref new CanvasTextFormat();
        this->font->WordWrapping = CanvasWordWrapping::NoWrap;
    }
}

void Textlet::FillExtent(float x, float y, float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    TextExtent ts = Win2DCanvas::GetTextExtent(content, font);

    if (width != nullptr) (*width) = ts.Width;
    if (height != nullptr) (*height) = ts.Height;
    if (descent != nullptr) (*descent) = ts.Descent;
    if (space != nullptr) (*space) = ts.Space;
    if (lspace != nullptr) (*lspace) = ts.LSpace;
    if (rspace != nullptr) (*rspace) = ts.RSpace;
};

void Textlet::Draw(CanvasDrawingSession^ ds, float x, float y) {
    ds->DrawText(content, (float)x, (float)y, Colors::Black, font);
}
