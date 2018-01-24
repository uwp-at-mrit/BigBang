#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>
#include <ppltasks.h>

#include "planet.hpp"
#include "syslog.hpp"
#include "system.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "transformation.hpp"

#include "snip/snip.hpp"
#include "decorator/decorator.hpp"
#include "interaction/listener.hpp"

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

class PlaceHolderListener : public IUniverseListener {};
class PlaceHolderDecorator : public IUniverseDecorator {};

class SnipInfo : public WarGrey::SCADA::ISnipInfo {
public:
    SnipInfo(IPlanet* master) : ISnipInfo(master) {};

public:
    float x;
    float y;
    float rotation;
    bool selected;

public:
	ISnip* next;
	ISnip* prev;
};

static inline SnipInfo* bind_snip_owership(IPlanet* master, ISnip* snip, double degrees) {
    auto info = new SnipInfo(master);
    snip->info = info;

    while (degrees <  0.000) degrees += 360.0;
    while (degrees >= 360.0) degrees -= 360.0;
    info->rotation = float(degrees * M_PI / 180.0);

    snip->construct();

    return info;
}

static inline void unsafe_fill_snip_bound(ISnip* snip, SnipInfo* info, float* x, float* y, float* width, float* height) {
	snip->fill_extent(info->x, info->y, width, height);

	(*x) = info->x;
	(*y) = info->y;

	if (info->rotation != 0.0F) {
		// TODO: the resulting rectangle is inaccurate especially for small snips.
		auto cx = (*x) + (*width) * 0.5F;
		auto cy = (*y) + (*height) * 0.5F;
		auto clip = rectangle((*x), (*y), (*width), (*height));
		auto enclosing = clip->ComputeBounds(make_float3x2_rotation(info->rotation, float2(cx, cy)));

		(*x) = enclosing.X;
		(*y) = enclosing.Y;
		(*width) = enclosing.Width;
		(*height) = enclosing.Height;
	}
}

static inline void unsafe_move_snip_via_info(Planet* master, SnipInfo* info, float x, float y, bool absolute) {
    if (!absolute) {
        x += info->x;
        y += info->y;
    }

    if ((info->x != x) || (info->y != y)) {
        info->x = x;
        info->y = y;

        master->size_cache_invalid();
    }
}

static inline void unsafe_add_selected(IPlanet* master, IUniverseListener* listener, ISnip* snip, SnipInfo* info) {
	listener->before_select(master, snip, true);
	info->selected = true;
	listener->after_select(master, snip, true);
}

static inline void unsafe_set_selected(IPlanet* master, IUniverseListener* listener, ISnip* snip, SnipInfo* info) {
	master->no_selected();
	unsafe_add_selected(master, listener, snip, info);
}

static inline void snip_center_point_offset(ISnip* snip, float width, float height, SnipCenterPoint& cp, float& xoff, float& yoff) {
    xoff = 0.0F;
    yoff = 0.0F;

    if (cp != SnipCenterPoint::LT) {
		float halfw = width  * 0.5F;
		float halfh = height * 0.5F;

        switch (cp) {
        case SnipCenterPoint::LC:               yoff = halfh;  break;
        case SnipCenterPoint::LB:               yoff = height; break;
        case SnipCenterPoint::CT: xoff = halfw;                break;
        case SnipCenterPoint::CC: xoff = halfw; yoff = halfh;  break;
        case SnipCenterPoint::CB: xoff = halfw; yoff = height; break;
        case SnipCenterPoint::RT: xoff = width;                break;
        case SnipCenterPoint::RC: xoff = width; yoff = halfh;  break;
        case SnipCenterPoint::RB: xoff = width; yoff = height; break;
        }
    }
}

/*************************************************************************************************/
Planet::Planet() : IPlanet() {
    this->set_decorator(nullptr);
	this->set_pointer_listener(nullptr);
}

Planet::~Planet() {
	this->collapse();
	this->listener->destroy();
    this->decorator->destroy();
}

void Planet::insert(ISnip* snip, double degrees, float x, float y) {
    if (snip->info == nullptr) {
		SnipInfo* info = bind_snip_owership(this, snip, degrees);
		
		if (this->head_snip == nullptr) {
            this->head_snip = snip;
            info->prev = this->head_snip;
        } else {
			SnipInfo* head_info = SNIP_INFO(this->head_snip);
			SnipInfo* prev_info = SNIP_INFO(head_info->prev);
			
			info->prev = head_info->prev;
            prev_info->next = snip;
            head_info->prev = snip;
        }
        info->next = this->head_snip;

        unsafe_move_snip_via_info(this, info, x, y, true);
        this->size_cache_invalid();
    }
}

void Planet::move_to(ISnip* snip, float x, float y, SnipCenterPoint cp) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this) {
            SnipInfo* info = SNIP_INFO(snip);
            float sx, sy, sw, sh, xoff, yoff;

			unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
            snip_center_point_offset(snip, sw, sh, cp, xoff, yoff);
            unsafe_move_snip_via_info(this, info, x - xoff, y - yoff, true);
        }
    }
}

void Planet::move(ISnip* snip, float x, float y) {
    if (snip != nullptr) {
		if ((snip->info != nullptr) && (snip->info->master == this)) {
            SnipInfo* info = SNIP_INFO(snip);

            unsafe_move_snip_via_info(this, info, x, y, false);
        }
    } else if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;

        do {
            SnipInfo* info = SNIP_INFO(child);

            if (info->selected) {
                unsafe_move_snip_via_info(this, info, x, y, false);
            }

            child = info->next;
        } while (child != this->head_snip);
    }
}

ISnip* Planet::find_snip(float x, float y) {
    float sx, sy, sw, sh;
    ISnip* found = nullptr;

    if (this->head_snip != nullptr) {
		SnipInfo* head_info = SNIP_INFO(this->head_snip);
        ISnip* child = head_info->prev;

        do {
            SnipInfo* info = SNIP_INFO(child);

            unsafe_fill_snip_bound(child, info, &sx, &sy, &sw, &sh);

            if ((sx < x) && (x < (sx + sw)) && (sy < y) && (y < (sy + sh))) {
                found = child;
                break;
            }

            child = info->prev;
        } while (child != head_info->prev);
    }

    return found;
}

void Planet::fill_snip_location(ISnip* snip, float* x, float* y, SnipCenterPoint cp) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this) {
            SnipInfo* info = SNIP_INFO(snip);
            float sx, sy, sw, sh, xoff, yoff;

			unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
            snip_center_point_offset(snip, sw, sh, cp, xoff, yoff);
            SET_BOX(x, sx + xoff);
            SET_BOX(y, sy + yoff);
        }
    }
}

void Planet::fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) {
	if ((snip != nullptr) && (snip->info != nullptr)) {
		if (snip->info->master == this) {
			SnipInfo* info = SNIP_INFO(snip);
			float sx, sy, sw, sh;
			
			unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
			SET_VALUES(x, sx, y, sy);
			SET_VALUES(width, sw, height, sh);
		}
	}
}

void Planet::fill_snips_bounds(float* x, float* y, float* width, float* height) {
    this->recalculate_snips_extent_when_invalid();
    SET_VALUES(x, this->snips_left, y, this->snips_top);
    SET_BOX(width, this->snips_right - this->snips_left);
    SET_BOX(height, this->snips_bottom - this->snips_top);
}

void Planet::size_cache_invalid() {
    this->snips_right = this->snips_left - 1.0F;
}

void Planet::recalculate_snips_extent_when_invalid() {
    if (this->snips_right < this->snips_left) {
        float rx, ry, width, height;

        if (this->head_snip == nullptr) {
            this->snips_left = 0.0F;
            this->snips_top = 0.0F;
            this->snips_right = 0.0F;
            this->snips_bottom = 0.0F;
        } else {
            ISnip* child = this->head_snip;

            this->snips_left = FLT_MAX;
            this->snips_top = FLT_MAX;
            this->snips_right = -FLT_MAX;
            this->snips_bottom = -FLT_MAX;

            do {
                SnipInfo* info = SNIP_INFO(child);

                unsafe_fill_snip_bound(child, info, &rx, &ry, &width, &height);
                this->snips_left = min(this->snips_left, rx);
                this->snips_top = min(this->snips_top, ry);
                this->snips_right = max(this->snips_right, rx + width);
                this->snips_bottom = max(this->snips_bottom, ry + height);

                child = info->next;
            } while (child != this->head_snip);
        }

        this->info->master->min_width = max(this->snips_right, this->preferred_min_width);
        this->info->master->min_height = max(this->snips_bottom, this->preferred_min_height);
    }
}

void Planet::add_selected(ISnip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this)) {
            SnipInfo* info = SNIP_INFO(snip);

            if ((!info->selected) && (this->rubberband_allowed && this->listener->can_select(this, snip))) {
                unsafe_add_selected(this, this->listener, snip, info);
            }
        }
    }
}

void Planet::set_selected(ISnip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this)) {
            SnipInfo* info = SNIP_INFO(snip);

            if ((!info->selected) && (this->listener->can_select(this, snip))) {
				unsafe_set_selected(this, this->listener, snip, info);
            }
        }
    }
}

void Planet::no_selected() {
	if (this->head_snip != nullptr) {
		ISnip* child = this->head_snip;

		do {
			SnipInfo* info = SNIP_INFO(child);

            if (info->selected) {
				this->listener->before_select(this, child, false);
				info->selected = false;
				this->listener->after_select(this, child, false);
			}

			child = info->next;
		} while (child != this->head_snip);
	}
}

/************************************************************************************************/
void Planet::on_pointer_moved(UIElement^ master, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        auto ppt = e->GetCurrentPoint(master);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;

        if (ppt->Properties->IsLeftButtonPressed) {
            if (this->rubberband_y == nullptr) {
				// TODO: implement interactive moving
                this->move(nullptr, x - this->last_pointer_x, y - this->last_pointer_y);
				this->last_pointer_x = x;
				this->last_pointer_y = y;
            } else {
                (*this->rubberband_x) = x;
                (*this->rubberband_y) = y;
            }
        }

        e->Handled = true;
    }
}

void Planet::on_pointer_pressed(UIElement^ master, PointerRoutedEventArgs^ e) {
	if ((!e->Handled) && (master->CapturePointer(e->Pointer))) {
		auto ppt = e->GetCurrentPoint(master);
		float x = ppt->Position.X;
		float y = ppt->Position.Y;

		if (ppt->Properties->IsLeftButtonPressed) {
			ISnip* snip = this->find_snip(x, y);

			this->last_pointer_x = x;
			this->last_pointer_y = y;

			if (snip == nullptr) {
				this->rubberband_x[0] = x;
				this->rubberband_x[1] = y;
				this->rubberband_y = this->rubberband_allowed ? (this->rubberband_x + 1) : nullptr;
				this->no_selected();
			} else {
				SnipInfo* info = SNIP_INFO(snip);
				
				this->rubberband_y = nullptr;

				if ((!info->selected) && this->listener->can_select(this, snip)) {
					if (e->KeyModifiers == VirtualKeyModifiers::Shift) {
						if (this->rubberband_allowed) {
							unsafe_add_selected(this, this->listener, snip, info);
						}
					} else {
						unsafe_set_selected(this, this->listener, snip, info);
					}
				}
			}
		} else {
			this->no_selected();
		}

		e->Handled = true;
    }
}

void Planet::on_pointer_released(UIElement^ master, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        if (this->rubberband_y != nullptr) {
            this->rubberband_y = nullptr;
			// TODO: select all touched snips
        }

        e->Handled = true;
    }
}

/*************************************************************************************************/
void Planet::set_pointer_listener(IUniverseListener* listener) {
    if (this->listener != nullptr) {
		this->listener->destroy();
    }

	this->listener = ((listener == nullptr) ? new PlaceHolderListener() : listener);
    this->listener->reference();
    this->rubberband_allowed = this->listener->can_select_multiple(this);
}

void Planet::set_decorator(IUniverseDecorator* decorator) {
    if (this->decorator != nullptr) {
        this->decorator->destroy();
    }

	this->decorator = ((decorator == nullptr) ? new PlaceHolderDecorator() : decorator);
    this->decorator->reference();
}

/*************************************************************************************************/
void Planet::construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float Width, float Height) {
    if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;

        do {
            child->construct();
            child = SNIP_INFO(child)->next;
        } while (child != this->head_snip);
    }
}

void Planet::update(long long count, long long interval, long long uptime, bool is_slow) {
    if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;

        do {
            child->update(count, interval, uptime, is_slow);
            child = SNIP_INFO(child)->next;
        } while (child != this->head_snip);
    }
}

void Planet::draw(CanvasDrawingSession^ ds, float Width, float Height) {
    CanvasActiveLayer^ layer = nullptr;
    float3x2 transform = ds->Transform;
    float width, height;
    
    this->decorator->draw_before(this, ds, Width, Height);

    if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;

        do {
            SnipInfo* info = SNIP_INFO(child);

            child->fill_extent(info->x, info->y, &width, &height);
            if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
                if (info->rotation == 0.0F) {
                    layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                } else {
					float cx = info->x + width * 0.5F;
					float cy = info->y + height * 0.5F;
					
					ds->Transform = make_rotation_matrix(info->rotation, cx, cy, transform.m31, transform.m32);
					layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                }

				this->decorator->draw_before_snip(child, ds, info->x, info->y, width, height);
				child->draw(ds, info->x, info->y, width, height);
				this->decorator->draw_after_snip(child, ds, info->x, info->y, width, height);

				if (info->selected) {
					ds->DrawRectangle(info->x, info->y, width, height, system_color(UIElementType::HighlightText));
				}

                delete layer; // Must Close the Layer Explicitly, it is C++/CX's quirk.
                ds->Transform = transform;
            }

            child = info->next;
        } while (child != this->head_snip);
    }

    if (this->rubberband_y != nullptr) {
        static auto rubberband_color = make_solid_brush(system_color(UIElementType::Highlight));

        float left = min(this->last_pointer_x, (*this->rubberband_x));
        float top = min(this->last_pointer_y, (*this->rubberband_y));
        float width = std::abs((*this->rubberband_x) - this->last_pointer_x);
        float height = std::abs((*this->rubberband_y) - this->last_pointer_y);

        rubberband_color->Opacity = 0.32F;
        ds->FillRectangle(left, top, width, height, rubberband_color);
        rubberband_color->Opacity = 1.00F;
        ds->DrawRectangle(left, top, width, height, rubberband_color);
    }

    this->decorator->draw_after(this, ds, Width, Height);
}

void Planet::collapse() {
	if (this->head_snip != nullptr) {
		ISnip* temp_head = this->head_snip;
		SnipInfo* temp_info = SNIP_INFO(temp_head);
		SnipInfo* prev_info = SNIP_INFO(temp_info->prev);
		
		this->head_snip = nullptr;
		prev_info->next = nullptr;
		
		do {
			ISnip* child = temp_head;

			temp_head = SNIP_INFO(temp_head)->next;
			delete child; // snip's destructor will delete the associated info object
		} while (temp_head != nullptr);
	}
}

/*************************************************************************************************/
void IPlanet::enter_critical_section() {
	this->section.lock();
}

void IPlanet::enter_shared_section() {
	this->section.lock_shared();
}

void IPlanet::leave_critical_section() {
	this->section.unlock();
}

void IPlanet::leave_shared_section() {
	this->section.unlock_shared();
}

void IPlanet::fill_actual_extent(float* width, float* height) {
	SET_BOX(width, this->info->master->actual_width);
	SET_BOX(height, this->info->master->actual_height);
}


CanvasRenderTarget^ IPlanet::take_snapshot(float width, float height, float dpi) {
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	CanvasRenderTarget^ snapshot = ref new CanvasRenderTarget(shared_dc, width, height, dpi);
	CanvasDrawingSession^ ds = snapshot->CreateDrawingSession();

	ds->Clear(ColorHelper::FromArgb(0, 0, 0, 0));
	this->enter_shared_section();
	this->draw(ds, width, height);
	this->leave_shared_section();

	return snapshot;
}

void IPlanet::save(Platform::String^ path, float width, float height, float dpi) {
	CanvasRenderTarget^ snapshot = this->take_snapshot(width, height, dpi);

	Concurrency::create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F))
		.then([=](Concurrency::task<void> saving) {
		try {
			saving.get();
		} catch (Platform::Exception^ e) {
			syslog(Log::Alert, "failed to save universe as bitmap:" + e->Message);
		}
	});
}
