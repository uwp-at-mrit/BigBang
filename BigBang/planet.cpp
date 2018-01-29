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

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
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
    SnipInfo(IPlanet* master, unsigned int mode)
		: ISnipInfo(master), mode(mode) {};

public:
    float x;
    float y;
    float rotation;

public:
    bool selected;
	float selected_x;
	float selected_y;

public:
	unsigned int mode;

public:
	ISnip* next;
	ISnip* prev;
};

static SnipInfo* bind_snip_owership(IPlanet* master, unsigned int mode, ISnip* snip, double degrees) {
    auto info = new SnipInfo(master, mode);
    snip->info = info;

    while (degrees <  0.000) degrees += 360.0;
    while (degrees >= 360.0) degrees -= 360.0;
    info->rotation = float(degrees * M_PI / 180.0);

    snip->construct();

    return info;
}

static inline SnipInfo* planet_snip_info(IPlanet* master, ISnip* snip) {
	SnipInfo* info = nullptr;

	if ((snip != nullptr) && (snip->info != nullptr)) {
		if (snip->info->master == master) {
			info = SNIP_INFO(snip);
		}
	}
	
	return info;
}

static inline bool unsafe_snip_unmasked(SnipInfo* info, unsigned int mode) {
	return ((info->mode == 0) || (info->mode == mode));
}

static void unsafe_fill_snip_bound(ISnip* snip, SnipInfo* info, float* x, float* y, float* width, float* height) {
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

static void unsafe_move_snip_via_info(Planet* master, SnipInfo* info, float x, float y, bool absolute) {
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

static inline void unsafe_add_selected(IPlanet* master, IUniverseListener* listener, ISnip* snip, SnipInfo* info, float x, float y) {
	listener->before_select(master, snip, true, x, y);
	info->selected = true;
	info->selected_x = x;
	info->selected_y = y;
	listener->after_select(master, snip, true, x, y);
}

static inline void unsafe_set_selected(IPlanet* master, IUniverseListener* listener, ISnip* snip, SnipInfo* info, float x, float y) {
	master->no_selected();
	unsafe_add_selected(master, listener, snip, info, x, y);
}

static void snip_center_point_offset(ISnip* snip, float width, float height, SnipCenterPoint& cp, float& xoff, float& yoff) {
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
Planet::Planet(Platform::String^ name, unsigned int initial_mode) : IPlanet(name), mode(initial_mode) {
    this->set_decorator(nullptr);
	this->set_pointer_listener(nullptr);
}

Planet::~Planet() {
	this->collapse();
	this->listener->destroy();
    this->decorator->destroy();
}

void Planet::change_mode(unsigned int mode) {
	if (mode != this->mode) {
		this->no_selected();
		this->mode = mode;
		this->size_cache_invalid();
	}
}

bool Planet::snip_unmasked(ISnip* snip) {
	SnipInfo* info = planet_snip_info(this, snip);

	return ((info != nullptr) && unsafe_snip_unmasked(info, this->mode));
}

void Planet::insert(ISnip* snip, double degrees, float x, float y) {
	if (snip->info == nullptr) {
		SnipInfo* info = bind_snip_owership(this, this->mode, snip, degrees);
		
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
	SnipInfo* info = planet_snip_info(this, snip);
	
	if ((info != nullptr) && unsafe_snip_unmasked(info, this->mode)) {
		float sx, sy, sw, sh, xoff, yoff;
		
		unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
		snip_center_point_offset(snip, sw, sh, cp, xoff, yoff);
		unsafe_move_snip_via_info(this, info, x - xoff, y - yoff, true);
	}
}

void Planet::move(ISnip* snip, float x, float y) {
	SnipInfo* info = planet_snip_info(this, snip);

    if (info != nullptr) {
		if (unsafe_snip_unmasked(info, this->mode)) {
			unsafe_move_snip_via_info(this, info, x, y, false);
		}
    } else if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;

        do {
            SnipInfo* info = SNIP_INFO(child);

            if (info->selected && unsafe_snip_unmasked(info, this->mode)) {
                unsafe_move_snip_via_info(this, info, x, y, false);
            }

            child = info->next;
        } while (child != this->head_snip);
    }
}

ISnip* Planet::find_snip(float x, float y) {
    ISnip* found = nullptr;

    if (this->head_snip != nullptr) {
		SnipInfo* head_info = SNIP_INFO(this->head_snip);
        ISnip* child = head_info->prev;

        do {
            SnipInfo* info = SNIP_INFO(child);

			if (unsafe_snip_unmasked(info, this->mode)) {
				float sx, sy, sw, sh;

				unsafe_fill_snip_bound(child, info, &sx, &sy, &sw, &sh);

				if ((sx < x) && (x < (sx + sw)) && (sy < y) && (y < (sy + sh))) {
					found = child;
					break;
				}
			}

            child = info->prev;
        } while (child != head_info->prev);
    }

    return found;
}

void Planet::fill_snip_location(ISnip* snip, float* x, float* y, SnipCenterPoint cp) {
	SnipInfo* info = planet_snip_info(this, snip);
	
	if ((info != nullptr) && unsafe_snip_unmasked(info, this->mode)) {
		float sx, sy, sw, sh, xoff, yoff;

		unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
		snip_center_point_offset(snip, sw, sh, cp, xoff, yoff);
		SET_BOX(x, sx + xoff);
		SET_BOX(y, sy + yoff);
    }
}

void Planet::fill_snip_bound(ISnip* snip, float* x, float* y, float* width, float* height) {
	SnipInfo* info = planet_snip_info(this, snip);

	if ((info != nullptr) && unsafe_snip_unmasked(info, this->mode)) {
		float sx, sy, sw, sh;
			
		unsafe_fill_snip_bound(snip, info, &sx, &sy, &sw, &sh);
		SET_VALUES(x, sx, y, sy);
		SET_VALUES(width, sw, height, sh);
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

				if (unsafe_snip_unmasked(info, this->mode)) {
					unsafe_fill_snip_bound(child, info, &rx, &ry, &width, &height);
					this->snips_left = min(this->snips_left, rx);
					this->snips_top = min(this->snips_top, ry);
					this->snips_right = max(this->snips_right, rx + width);
					this->snips_bottom = max(this->snips_bottom, ry + height);
				}

                child = info->next;
            } while (child != this->head_snip);
        }

        this->info->master->min_width = max(this->snips_right, this->preferred_min_width);
        this->info->master->min_height = max(this->snips_bottom, this->preferred_min_height);
    }
}

void Planet::add_selected(ISnip* snip) {
	if (this->rubberband_allowed) {
		SnipInfo* info = planet_snip_info(this, snip);

		if ((info != nullptr) && (!info->selected)) {
			if (unsafe_snip_unmasked(info, this->mode) && this->listener->can_select(this, snip, 0.0F, 0.0F)) {
				unsafe_add_selected(this, this->listener, snip, info, 0.0F, 0.0F);
			}
		}
	}
}

void Planet::set_selected(ISnip* snip) {
	SnipInfo* info = planet_snip_info(this, snip);

    if ((info != nullptr) && (!info->selected)) {
		if (unsafe_snip_unmasked(info, this->mode) && (this->listener->can_select(this, snip, 0.0F, 0.0F))) {
			unsafe_set_selected(this, this->listener, snip, info, 0.0F, 0.0F);
		}
    }
}

void Planet::no_selected() {
	if (this->head_snip != nullptr) {
		ISnip* child = this->head_snip;

		do {
			SnipInfo* info = SNIP_INFO(child);

            if (info->selected && unsafe_snip_unmasked(info, this->mode)) {
				this->listener->before_select(this, child, false, info->selected_x, info->selected_y);
				info->selected = false;
				this->listener->after_select(this, child, false, info->selected_x, info->selected_y);
			}

			child = info->next;
		} while (child != this->head_snip);
	}
}

/************************************************************************************************/
bool Planet::on_pointer_moved(PointerPoint^ ppt, VirtualKeyModifiers vkms) {
	bool handled = false;
	
	if (ppt->Properties->IsLeftButtonPressed) {
		float x = ppt->Position.X;
		float y = ppt->Position.Y;

		if (this->rubberband_y == nullptr) {
			// TODO: implement interactive moving
			this->move(nullptr, x - this->last_pointer_x, y - this->last_pointer_y);
			this->last_pointer_x = x;
			this->last_pointer_y = y;
		} else {
			(*this->rubberband_x) = x;
			(*this->rubberband_y) = y;
		}

		handled = true;
	}

	return handled;
}

bool Planet::on_pointer_pressed(PointerPoint^ ppt, VirtualKeyModifiers vkms) {
	bool handled = false;
	
	if (ppt->Properties->IsLeftButtonPressed) {
		float x = ppt->Position.X;
		float y = ppt->Position.Y;
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
			float selected_x = x - info->x;
			float selected_y = y - info->y;

			this->rubberband_y = nullptr;

			if (unsafe_snip_unmasked(info, this->mode)) {
				if ((!info->selected) && this->listener->can_select(this, snip, selected_x, selected_y)) {
					if (vkms == VirtualKeyModifiers::Shift) {
						if (this->rubberband_allowed) {
							unsafe_add_selected(this, this->listener, snip, info, selected_x, selected_y);
						}
					} else {
						unsafe_set_selected(this, this->listener, snip, info, selected_x, selected_y);
					}
				}
			}
		}

		handled = true;
	} else {
		this->no_selected();

		handled = true;
	}

	return handled;
}

bool Planet::on_pointer_released(PointerPoint^ ppt, VirtualKeyModifiers vkms) {
	bool handled = false;

	if (this->rubberband_y != nullptr) {
		this->rubberband_y = nullptr;
		// TODO: select all touched snips

		handled = true;
	}

	return handled;
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
void Planet::construct(CanvasCreateResourcesReason reason, float Width, float Height) {
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
			SnipInfo* info = SNIP_INFO(child);

			if (unsafe_snip_unmasked(info, this->mode)) {
				child->update(count, interval, uptime, is_slow);
			}
			
			child = info->next;
        } while (child != this->head_snip);
    }
}

void Planet::draw(CanvasDrawingSession^ ds, float Width, float Height) {
	CanvasActiveLayer^ layer = nullptr;
    float3x2 transform = ds->Transform;
	float transformX = transform.m31;
	float transformY = transform.m32;
	float dsX = abs(min(0.0F, transformX));
	float dsY = abs(min(0.0F, transformY));
	float dsWidth = Width - max(transformX, 0.0F);
	float dsHeight = Height - max(transformY, 0.0F);

    this->decorator->draw_before(this, ds, Width, Height);

    if (this->head_snip != nullptr) {
        ISnip* child = this->head_snip;
		float width, height;

        do {
            SnipInfo* info = SNIP_INFO(child);

			if (unsafe_snip_unmasked(info, this->mode)) {
				child->fill_extent(info->x, info->y, &width, &height);
				if ((info->x < dsWidth) && (info->y < dsHeight) && ((info->x + width) > dsX) && ((info->y + height) > dsY)) {
					if (info->rotation == 0.0F) {
						layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
					} else {
						float cx = info->x + width * 0.5F;
						float cy = info->y + height * 0.5F;

						ds->Transform = make_rotation_matrix(info->rotation, cx, cy, transformX, transformY);
						layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
					}

					this->decorator->draw_before_snip(child, ds, info->x, info->y, width, height);
					child->draw(ds, info->x, info->y, width, height);
					this->decorator->draw_after_snip(child, ds, info->x, info->y, width, height);

					if (info->selected) {
						this->decorator->draw_selected_snip(child, ds, info->x, info->y, width, height);
					}

					delete layer; // Must Close the Layer Explicitly, it is C++/CX's quirk.
					ds->Transform = transform;
				}
			}

            child = info->next;
        } while (child != this->head_snip);
    }

    if (this->rubberband_y != nullptr) {
        static auto rubberband_color = make_solid_brush(system_color(UIElementType::Highlight));

        float left = min(this->last_pointer_x, (*this->rubberband_x));
        float top = min(this->last_pointer_y, (*this->rubberband_y));
        float width = abs((*this->rubberband_x) - this->last_pointer_x);
        float height = abs((*this->rubberband_y) - this->last_pointer_y);

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
IPlanet::~IPlanet() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
};

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
