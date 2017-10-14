#define _USE_MATH_DEFINES
#include <algorithm>
#include <WindowsNumerics.h>

#include "control.hxx"
#include "universe.hpp"

#include "rsyslog.hpp"
#include "system.hpp"
#include "geometry.hpp"
#include "snip/snip.hpp"
#include "decorator/decorator.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Devices::Input;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/** NOTE
 *   C-Style casting tries all C++ style casting except dynamic_cast;
 *   reinterpret_cast may cause "Access violation reading location 0xFFFFFFFFFFFFFFFF" even for subtype casting.
 */

#define SNIP_INFO(snip) (static_cast<SnipInfo*>(snip->info))
#define REMOVE(ptr, refcount) if (ptr->refcount <= 1) { delete ptr; } else { ptr->refcount -= 1; }

ref class Win2DUniverse;

//class PlaceHolderListener : public IUniverseListener {};
class PlaceHolderDecorator : public IUniverseDecorator {};

class SnipInfo : public WarGrey::SCADA::ISnipInfo {
public:
    SnipInfo(Win2DControl^ master) : ISnipInfo(master) {};

public:
    float x;
    float y;
    float rotation;
    bool selected;
};

static inline void fill_snip_extent(Snip* snip, SnipInfo* info, float* x, float* y, float* width, float* height) {
    snip->fill_extent(info->x, info->y, width, height);

    (*x) = info->x;
    (*y) = info->y;

    if (info->rotation != 0.0F) {
        // TODO: the resulting rectangle is inaccurate especially for small snips.
        auto cx = (*x) + (*width) / 2.0F;
        auto cy = (*y) + (*height) / 2.0F;
        auto clip = rectangle((*x), (*y), (*width), (*height));
        auto enclosing = clip->ComputeBounds(make_float3x2_rotation(info->rotation, float2(cx, cy)));

        (*x) = enclosing.X;
        (*y) = enclosing.Y;
        (*width) = enclosing.Width;
        (*height) = enclosing.Height;
    }
}

static inline SnipInfo* bind_snip_owership(Win2DControl^ master, Snip* snip, double degrees) {
    auto info = new SnipInfo(master);
    snip->info = info;

    while (degrees <  0.000) degrees += 360.0;
    while (degrees >= 360.0) degrees -= 360.0;
    info->rotation = float(degrees * M_PI / 180.0);

    return info;
}

static inline void unsafe_move_snip_via_info(SnipInfo* info, float x, float y, bool absolute) {
    if (!absolute) {
        x += info->x;
        y += info->y;
    }

    if ((info->x != x) || (info->y != y)) {
        info->x = x;
        info->y = y;

        //info->master->size_cache_invalid();
        //info->master->refresh();
    }
}

static inline void unsafe_add_selected(IUniverseListener* listener, Snip* snip, SnipInfo* info) {
    //listener->before_select(info->master, snip);
    info->selected = true;
    //info->master->refresh();
    //listener->after_select(info->master, snip);
}

static inline void unsafe_set_selected(IUniverseListener* listener, Snip* snip, SnipInfo* info) {
    unsafe_add_selected(listener, snip, info);
}

/*************************************************************************************************/
Universe::Universe(Panel^ parent, int frame_rate) : IUniverse(parent, frame_rate) {
    //this->set_pointer_listener(nullptr);
    this->set_decorator(nullptr);
}

Universe::~Universe() {
    if (this->head_snip != nullptr) {
        Snip* child = nullptr;
        this->head_snip->prev->next = nullptr;
        do {
            child = this->head_snip;
            this->head_snip = this->head_snip->next;
            delete child; // snip's destructor will delete the associated info object
        } while (this->head_snip != nullptr);
    }

    //REMOVE(this->listener, refcount);
    REMOVE(this->decorator, refcount);
}

void Universe::insert(Snip* snip, float x, float y, double degrees) {
    if (snip->info == nullptr) { // TODO: should it be copied if one snip can only belongs to one pasteboard
        if (this->head_snip == nullptr) {
            this->head_snip = snip;
            snip->prev = this->head_snip;
        } else {
            snip->prev = this->head_snip->prev;
            this->head_snip->prev->next = snip;
            this->head_snip->prev = snip;
        }
        snip->next = this->head_snip;

        auto info = bind_snip_owership(this->master, snip, degrees);
        unsafe_move_snip_via_info(info, x, y, true);
        this->size_cache_invalid();
    }
}

void Universe::move_to(Snip* snip, float x, float y) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this->master) {
            SnipInfo* info = SNIP_INFO(snip);
            unsafe_move_snip_via_info(info, x, y, true);
        }
    }
}

void Universe::move(Snip* snip, float x, float y) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this->master) {
            SnipInfo* info = SNIP_INFO(snip);
            unsafe_move_snip_via_info(info, x, y, false);
        }
    } else if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        //this->begin_edit_sequence();
        do {
            SnipInfo* info = SNIP_INFO(child);
            if (info->selected) {
                unsafe_move_snip_via_info(info, x, y, false);
            }
            child = child->next;
        } while (child != this->head_snip);
        //this->end_edit_sequence();
    }
}

Snip* Universe::find_snip(float x, float y) {
    float sx, sy, sw, sh;
    Snip* found = nullptr;

    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip->prev;

        do {
            SnipInfo* info = SNIP_INFO(child);
            fill_snip_extent(child, info, &sx, &sy, &sw, &sh);

            if ((sx < x) && (x < (sx + sw)) && (sy < y) && (y < (sy + sh))) {
                found = child;
                break;
            }

            child = child->prev;
        } while (child != this->head_snip->prev);
    }

    return found;
}

void Universe::fill_snips_bounds(float* x, float* y, float* width, float* height) {
    this->recalculate_snips_extent_when_invalid();
    if (x != nullptr) (*x) = this->snips_left;
    if (y != nullptr) (*y) = this->snips_top;
    if (width != nullptr) (*width) = this->snips_right - this->snips_left;
    if (height != nullptr) (*height) = this->snips_bottom - this->snips_top;
}

void Universe::size_cache_invalid() {
    this->snips_right = this->snips_left - 1.0F;
}

void Universe::recalculate_snips_extent_when_invalid() {
    if (this->snips_right < this->snips_left) {
        float rx, ry, width, height;

        if (this->head_snip == nullptr) {
            this->snips_left = 0.0F;
            this->snips_top = 0.0F;
            this->snips_right = 0.0F;
            this->snips_bottom = 0.0F;
        } else {
            Snip* child = this->head_snip;

            this->snips_left = FLT_MAX;
            this->snips_top = FLT_MAX;
            this->snips_right = -FLT_MAX;
            this->snips_bottom = -FLT_MAX;

            do {
                SnipInfo* info = SNIP_INFO(child);
                fill_snip_extent(child, info, &rx, &ry, &width, &height);
                this->snips_left = std::min(this->snips_left, rx);
                this->snips_top = std::min(this->snips_top, ry);
                this->snips_right = std::max(this->snips_right, rx + width);
                this->snips_bottom = std::max(this->snips_bottom, ry + height);

                child = child->next;
            } while (child != this->head_snip);
        }

        this->master->min_width = std::max(this->snips_right, this->preferred_min_width);
        this->master->min_height = std::max(this->snips_bottom, this->preferred_min_height);
    }
}

void Universe::add_selected(Snip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this->master)) {
            SnipInfo* info = SNIP_INFO(snip);
            if ((!info->selected) && (this->rubberband_allowed /*&& this->listener->can_select(this, snip)*/)) {
                unsafe_add_selected(this->listener, snip, info);
            }
        }
    }
}

void Universe::set_selected(Snip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this->master)) {
            SnipInfo* info = SNIP_INFO(snip);
            if ((!info->selected) /*&& (this->listener->can_select(this, snip))*/) {
                unsafe_set_selected(this->listener, snip, info);
            }
        }
    }
}

void Universe::no_selected() {
    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        //this->begin_edit_sequence();
        do {
            SnipInfo* info = SNIP_INFO(child);
            if (info->selected) {
                //this->listener->before_deselect(this, child);
                info->selected = false;
                //this->refresh();
                //this->listener->after_deselect(this, child);
            }

            child = child->next;
        } while (child != this->head_snip);
        //this->end_edit_sequence();
    }
}

/************************************************************************************************/
void Universe::on_pointer_moved(UIElement^ control, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        auto ppt = e->GetCurrentPoint(control);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;

        if (ppt->Properties->IsLeftButtonPressed) {
            if (this->rubberband_y == nullptr) {
                //if (layout->can_move(this, e)) {
                //    this->move(nullptr, x - this->last_pointer_x, y - this->last_pointer_y);
                //    this->last_pointer_x = x;
                //    this->last_pointer_y = y;
                //}
            } else {
                (*this->rubberband_x) = x;
                (*this->rubberband_y) = y;
                //this->refresh();
            }
        }

        e->Handled = true;
    }
}

void Universe::on_pointer_pressed(UIElement^ control, PointerRoutedEventArgs^ e) {
    if ((!e->Handled) && (control->CapturePointer(e->Pointer))) {
        auto ppt = e->GetCurrentPoint(control);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;

        if (ppt->Properties->IsLeftButtonPressed) {
            Snip* snip = this->find_snip(x, y);

            this->last_pointer_x = x;
            this->last_pointer_y = y;

            if (snip == nullptr) {
                this->rubberband_y = this->rubberband_allowed ? (this->rubberband_x + 1) : nullptr;
                this->no_selected();
            } else {
                this->rubberband_y = nullptr;
                SnipInfo* info = SNIP_INFO(snip);
                if ((!info->selected) /*&& this->listener->can_select(this, snip)*/) {
                    if (e->KeyModifiers == VirtualKeyModifiers::Shift) {
                        if (this->rubberband_allowed) {
                            unsafe_add_selected(this->listener, snip, info);
                        }
                    } else {
                        unsafe_set_selected(this->listener, snip, info);
                    }
                }
            }
        } else {
            this->no_selected();
        }

        e->Handled = true;
    }
}

void Universe::on_pointer_released(UIElement^ control, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        if (this->rubberband_y != nullptr) {
            this->rubberband_y = nullptr;
        }

        e->Handled = true;
    }
}

/*************************************************************************************************/
//void Universe::set_pointer_listener(IUniverseListener* listener) {
//    if (this->listener != nullptr) {
        //REMOVE(this->listener, refcount);
//    }

    //this->listener = (listener == nullptr) ? new PlaceHolderListener() : listener;
    //this->listener->refcount += 1;
    //this->rubberband_allowed = this->listener->can_select_multiple(this);
//}

void Universe::set_decorator(IUniverseDecorator* decorator) {
    if (this->decorator != nullptr) {
        REMOVE(this->decorator, refcount);
    }

    this->decorator = (decorator == nullptr) ? new PlaceHolderDecorator() : decorator;
    this->decorator->refcount += 1;
    //this->refresh();
}

/*************************************************************************************************/
void Universe::update(long long count, long long interval, long long uptime, bool is_slow) {
    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        do {
            child->update(count, interval, uptime, is_slow);
            child = child->next;
        } while (child != this->head_snip);
    }
}

void Universe::draw(CanvasDrawingSession^ ds, float Width, float Height) {
    CanvasActiveLayer ^layer = nullptr;
    float3x2 transform = ds->Transform;
    float width, height;
    
    this->decorator->draw_before(this, ds, Width, Height);

    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        do {
            SnipInfo* info = SNIP_INFO(child);
            child->fill_extent(info->x, info->y, &width, &height);

            if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
                if (info->rotation == 0.0F) {
                    layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                } else {
                    auto cx = info->x + width / 2.0F;
                    auto cy = info->y + height / 2.0F;

                    ds->Transform = make_float3x2_rotation(info->rotation, float2(cx, cy));
                    layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                }

                child->draw(ds, info->x, info->y, width, height);

                delete layer; // Must Close the Layer Explicitly
                ds->Transform = transform;
            }

            child = child->next;
        } while (child != this->head_snip);
    }

    if (this->rubberband_y != nullptr) {
        static auto rubberband_color = ref new CanvasSolidColorBrush(ds, system_color(UIElementType::Highlight));

        float left = std::min(this->last_pointer_x, (*this->rubberband_x));
        float top = std::min(this->last_pointer_y, (*this->rubberband_y));
        float width = std::abs((*this->rubberband_x) - this->last_pointer_x);
        float height = std::abs((*this->rubberband_y) - this->last_pointer_y);

        rubberband_color->Opacity = 0.32F;
        ds->FillRectangle(left, top, width, height, rubberband_color);
        rubberband_color->Opacity = 1.00F;
        ds->DrawRectangle(left, top, width, height, rubberband_color);
    }

    this->decorator->draw_after(this, ds, Width, Height);
}

/*************************************************************************************************/
private ref class Win2DUniverse sealed : public WarGrey::SCADA::Win2DControl {
internal:
    Win2DUniverse(IUniverse* world, int frame_rate, Panel^ parent, Platform::String^ id = nullptr) {
        this->world = world; 
        this->planet = ref new CanvasAnimatedControl();
        if (id != nullptr) this->planet->Name = id;

        if (frame_rate > 0) {
            this->planet->TargetElapsedTime = TimeSpan({ 10000000LL / frame_rate });
        } else if (frame_rate < 0) {
            this->planet->TargetElapsedTime = TimeSpan({ -10000000LL * frame_rate });
        }

        this->planet->UseSharedDevice = true; // this is required
        this->planet->SizeChanged += ref new SizeChangedEventHandler(this, &Win2DUniverse::do_resize);
        this->planet->CreateResources += ref new UniverseLoadHandler(this, &Win2DUniverse::do_load);
        this->planet->GameLoopStarting += ref new UniverseHandler(this, &Win2DUniverse::do_start);
        this->planet->GameLoopStopped += ref new UniverseHandler(this, &Win2DUniverse::do_stop);
        this->planet->Update += ref new UniverseUpdateHandler(this, &Win2DUniverse::do_update);
        this->planet->Draw += ref new UniverseDrawHandler(this, &Win2DUniverse::do_paint);

        this->planet->PointerMoved += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_moved);
        this->planet->PointerPressed += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_pressed);
        this->planet->PointerReleased += ref new PointerEventHandler(this, &Win2DUniverse::on_pointer_released);

        Win2DControl::universe = this->planet;
        Win2DControl::control = this->planet;

        parent->Children->Append(this->planet);
    }

private:
    void do_resize(Platform::Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ args) {
        float width = args->NewSize.Width;
        float height = args->NewSize.Height;

        if ((width > 0.0F) && (height > 0.0F)) {
            this->world->reflow(width, height);
        }
    }

    void do_start(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
        this->world->start();
    }

    void do_load(CanvasAnimatedControl^ sender, CanvasCreateResourcesEventArgs^ args) {
        this->world->load(args);
    }

    void do_update(ICanvasAnimatedControl^ sender, CanvasAnimatedUpdateEventArgs^ args) {
        long long count = args->Timing.UpdateCount - 1;
        long long elapsed = args->Timing.ElapsedTime.Duration;
        long long uptime = args->Timing.TotalTime.Duration;
        bool is_slow = args->Timing.IsRunningSlowly;

        this->world->update(count, elapsed, uptime, is_slow);
    }

    void do_paint(ICanvasAnimatedControl^ sender, CanvasAnimatedDrawEventArgs^ args) {
        Size region = this->planet->Size;

        try {
            this->world->draw(args->DrawingSession, region.Width, region.Height);
        } catch (Platform::Exception^ wte) {
            /// TODO: Why it complains about the WrongThreadException at first running.
            rsyslog(wte->Message);
        }
    }

    void do_stop(ICanvasAnimatedControl^ sender, Platform::Object^ args) {
        this->world->stop();
    }

private:
    void on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
        this->world->on_pointer_moved(Win2DControl::canvas, args);
    }

    void on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
        this->world->on_pointer_pressed(Win2DControl::canvas, args);
    }

    void on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
        this->world->on_pointer_released(Win2DControl::canvas, args);
    }

private:
    CanvasAnimatedControl^ planet;
    IUniverse* world;
};

IUniverse::IUniverse(Panel^ parent, int frame_rate) {
    this->master = ref new Win2DUniverse(this, frame_rate, parent, nullptr);
}

IUniverse::~IUniverse() {
    this->master = nullptr;
}

void IUniverse::resize(float width, float height) {
    if ((width != this->master->actual_width) || (height != this->master->actual_height)) {
        this->master->width = width;
        this->master->height = height;
    }
}
