#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>
#include <ppltasks.h>

#include "planet.hpp"
#include "syslog.hpp"
#include "system.hpp"
#include "tongue.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "brushes.hxx"
#include "transformation.hpp"

#include "graphlet/primitive.hpp"
#include "decorator/decorator.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Windows::Devices::Input;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/** NOTE
 *   C-Style casting tries all C++ style casting except dynamic_cast;
 *   reinterpret_cast may cause "Access violation reading location 0xFFFFFFFFFFFFFFFF" even for subtype casting.
 */

#define GRAPHLET_INFO(g) (static_cast<GraphletInfo*>(g->info))

class GraphletInfo : public WarGrey::SCADA::IGraphletInfo {
public:
    GraphletInfo(IPlanet* master, unsigned int mode) : IGraphletInfo(master), mode(mode) {};

public:
    float x;
    float y;
    float rotation;
    bool selected;

public:
	unsigned int mode;

public: // for asynchronously loaded graphlets
	float x0;
	float y0;
	GraphletAnchor anchor0;
	

public:
	IGraphlet* next;
	IGraphlet* prev;
};

static GraphletInfo* bind_graphlet_owership(IPlanet* master, unsigned int mode, IGraphlet* g, double degrees) {
    auto info = new GraphletInfo(master, mode);
    g->info = info;

    while (degrees <  0.000) degrees += 360.0;
    while (degrees >= 360.0) degrees -= 360.0;
    info->rotation = float(degrees * M_PI / 180.0);

    return info;
}

static inline GraphletInfo* planet_graphlet_info(IPlanet* master, IGraphlet* g) {
	GraphletInfo* info = nullptr;

	if ((g != nullptr) && (g->info != nullptr)) {
		if (g->info->master == master) {
			info = GRAPHLET_INFO(g);
		}
	}
	
	return info;
}

static inline bool unsafe_graphlet_unmasked(GraphletInfo* info, unsigned int mode) {
	return ((info->mode & mode) == info->mode);
}

static void unsafe_fill_graphlet_bound(IGraphlet* g, GraphletInfo* info, float* x, float* y, float* width, float* height) {
	g->fill_extent(info->x, info->y, width, height);

	(*x) = info->x;
	(*y) = info->y;

	if (info->rotation != 0.0F) {
		// TODO: the resulting rectangle is inaccurate especially for small graphlets.
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

static inline void unsafe_add_selected(IPlanet* master, IGraphlet* g, GraphletInfo* info) {
	master->before_select(g, true);
	info->selected = true;
	master->after_select(g, true);
	master->notify_graphlet_updated(g);
}

static inline void unsafe_set_selected(IPlanet* master, IGraphlet* g, GraphletInfo* info) {
	master->begin_update_sequence();
	master->no_selected();
	unsafe_add_selected(master, g, info);
	master->end_update_sequence();
}

static void graphlet_anchor_offset(IGraphlet* g, float width, float height, GraphletAnchor& a, float* xoff, float* yoff) {
    float dx = 0.0F;
    float dy = 0.0F;

    if (a != GraphletAnchor::LT) {
		float halfw = width  * 0.5F;
		float halfh = height * 0.5F;

        switch (a) {
        case GraphletAnchor::LC:             dy = halfh;  break;
        case GraphletAnchor::LB:             dy = height; break;
        case GraphletAnchor::CT: dx = halfw;              break;
        case GraphletAnchor::CC: dx = halfw; dy = halfh;  break;
        case GraphletAnchor::CB: dx = halfw; dy = height; break;
        case GraphletAnchor::RT: dx = width;              break;
        case GraphletAnchor::RC: dx = width; dy = halfh;  break;
        case GraphletAnchor::RB: dx = width; dy = height; break;
        }
    }

	(*xoff) = dx;
	(*yoff) = dy;
}

static bool unsafe_move_graphlet_via_info(Planet* master, GraphletInfo* info, float x, float y, bool absolute) {
	bool moved = false;
	
	if (!absolute) {
		x += info->x;
		y += info->y;
	}

	if ((info->x != x) || (info->y != y)) {
		info->x = x;
		info->y = y;

		master->size_cache_invalid();
		moved = true;
	}

	return moved;
}

static bool unsafe_move_graphlet_via_info(Planet* master, IGraphlet* g, GraphletInfo* info, float x, float y, GraphletAnchor a, bool absolute) {
	float sx, sy, sw, sh;
	float dx = 0.0F;
	float dy = 0.0F;

	if (g->ready()) {
		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		graphlet_anchor_offset(g, sw, sh, a, &dx, &dy);
	} else {
		info->x0 = x;
		info->y0 = y;
		info->anchor0 = a;
	}
	
	return unsafe_move_graphlet_via_info(master, info, x - dx, y - dy, true);
}

static IGraphlet* do_search_selected_graphlet(IGraphlet* start, unsigned int mode, IGraphlet* terminator) {
	IGraphlet* found = nullptr;
	IGraphlet* child = start;

	do {
		GraphletInfo* info = GRAPHLET_INFO(child);

		if (info->selected && (unsafe_graphlet_unmasked(info, mode))) {
			found = child;
			break;
		}

		child = info->next;
	} while (child != terminator);
	
	return found;
}

/*************************************************************************************************/
Planet::Planet(Platform::String^ name, unsigned int initial_mode) : IPlanet(name), mode(initial_mode), needs_update(false), update_sequence_depth(0) {
	this->numpad = new Numpad(this);
}

Planet::~Planet() {
	this->collapse();
    
	for (IPlanetDecorator* decorator : this->decorators) {
		delete decorator;
	}

	delete this->numpad;
}

void Planet::change_mode(unsigned int mode) {
	if (mode != this->mode) {
		this->no_selected();
		this->numpad->show(false);
		this->mode = mode;
		this->size_cache_invalid();
		this->notify_graphlet_updated(nullptr);
	}
}

bool Planet::graphlet_unmasked(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	return ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode));
}

void Planet::notify_graphlet_ready(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	if (info != nullptr) {
		this->size_cache_invalid();
		this->begin_update_sequence();

		/** TODO
		 * The moving may occur more than once in or not in the same thread,
		 *  do we need a mechanism to avoid the redundant ones?
		 */
		unsafe_move_graphlet_via_info(this, g, info, info->x0, info->y0, info->anchor0, true);
		
		this->notify_graphlet_updated(g);
		this->on_graphlet_ready(g);
		this->end_update_sequence();
	}
}

void Planet::notify_graphlet_updated(ISprite* g) { // NOTE: `g` may be `nullptr`
	if (this->in_update_sequence()) {
		this->needs_update = true;
	} else if (this->info != nullptr) {
		this->info->master->refresh(this);
		this->needs_update = false;
	}
}

void Planet::begin_update_sequence() {
	this->update_sequence_depth += 1;
}

bool Planet::in_update_sequence() {
	return (this->update_sequence_depth > 0);
}

void Planet::end_update_sequence() {
	this->update_sequence_depth -= 1;

	if (this->update_sequence_depth < 1) {
		this->update_sequence_depth = 0;

		if ((this->needs_update) && (this->info != nullptr)) {
			this->info->master->refresh(this);
			this->needs_update = false;
		}
	}
}

void Planet::insert(IGraphlet* g, float x, float y, GraphletAnchor a) {
	if (g->info == nullptr) {
		GraphletInfo* info = bind_graphlet_owership(this, this->mode, g, 0.0);

		if (this->head_graphlet == nullptr) {
            this->head_graphlet = g;
            info->prev = this->head_graphlet;
        } else {
			GraphletInfo* head_info = GRAPHLET_INFO(this->head_graphlet);
			GraphletInfo* prev_info = GRAPHLET_INFO(head_info->prev);
			
			info->prev = head_info->prev;
            prev_info->next = g;
            head_info->prev = g;
        }
        info->next = this->head_graphlet;

		g->construct();
		unsafe_move_graphlet_via_info(this, g, info, x, y, a, true);

		this->notify_graphlet_updated(g);
	}
}

void Planet::insert(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, GraphletAnchor a, float dx, float dy) {
	if (g->info == nullptr) {
		GraphletInfo* tinfo = planet_graphlet_info(this, target);
		float x = 0.0F;
		float y = 0.0F;

		// TODO: what if the target graphlet is not ready?

		if ((tinfo != nullptr) && unsafe_graphlet_unmasked(tinfo, this->mode)) {
			float sx, sy, sw, sh, xoff, yoff;

			unsafe_fill_graphlet_bound(target, tinfo, &sx, &sy, &sw, &sh);
			graphlet_anchor_offset(target, sw, sh, ta, &xoff, &yoff);
			x = sx + xoff + dx;
			y = sy + yoff + dy;
		}

		this->insert(g, x, y, a);
	}
}

void Planet::remove(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		GraphletInfo* prev_info = GRAPHLET_INFO(info->prev);
		GraphletInfo* next_info = GRAPHLET_INFO(info->next);

		prev_info->next = info->next;
		next_info->prev = info->prev;

		if (this->head_graphlet == g) {
			if (this->head_graphlet == info->next) {
				this->head_graphlet = nullptr;
			} else {
				this->head_graphlet = info->next;
			}
		}

		if (this->hovering_graphlet == g) {
			this->hovering_graphlet = nullptr;
		}
		
		delete g; // g's destructor will delete the associated info object
		this->notify_graphlet_updated(nullptr);
		this->size_cache_invalid();
	}
}

void Planet::erase() {
	if (this->head_graphlet != nullptr) {
		IGraphlet* temp_head = this->head_graphlet;
		GraphletInfo* temp_info = GRAPHLET_INFO(temp_head);
		GraphletInfo* prev_info = GRAPHLET_INFO(temp_info->prev);

		this->head_graphlet = nullptr;
		prev_info->next = nullptr;

		do {
			IGraphlet* child = temp_head;

			temp_head = GRAPHLET_INFO(temp_head)->next;

			delete child; // child's destructor will delete the associated info object
		} while (temp_head != nullptr);

		this->head_graphlet = nullptr;
		this->size_cache_invalid();
	}
}

void Planet::move_to(IGraphlet* g, float x, float y, GraphletAnchor a) {
	GraphletInfo* info = planet_graphlet_info(this, g);
	
	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		if (unsafe_move_graphlet_via_info(this, g, info, x, y, a, true)) {
			this->notify_graphlet_updated(g);
		}
	}
}

void Planet::move_to(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, GraphletAnchor a, float dx, float dy) {
	GraphletInfo* info = planet_graphlet_info(this, g);
	GraphletInfo* tinfo = planet_graphlet_info(this, target);

	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)
		&& (tinfo != nullptr) && unsafe_graphlet_unmasked(tinfo, this->mode)) {
		float sx, sy, sw, sh, xoff, yoff;

		unsafe_fill_graphlet_bound(target, tinfo, &sx, &sy, &sw, &sh);
		graphlet_anchor_offset(target, sw, sh, ta, &xoff, &yoff);
		
		if (unsafe_move_graphlet_via_info(this, g, info, sx + xoff + dx, sy + yoff + dy, a, true)) {
			this->notify_graphlet_updated(g);
		}
	}
}

void Planet::move(IGraphlet* g, float x, float y) {
	GraphletInfo* info = planet_graphlet_info(this, g);

    if (info != nullptr) {
		if (unsafe_graphlet_unmasked(info, this->mode)) {
			if (unsafe_move_graphlet_via_info(this, info, x, y, false)) {
				this->notify_graphlet_updated(g);
			}
		}
    } else if (this->head_graphlet != nullptr) {
        IGraphlet* child = this->head_graphlet;

        do {
            info = GRAPHLET_INFO(child);

            if (info->selected && unsafe_graphlet_unmasked(info, this->mode)) {
                unsafe_move_graphlet_via_info(this, info, x, y, false);
            }

            child = info->next;
        } while (child != this->head_graphlet);

		this->notify_graphlet_updated(nullptr);
    }
}

IGraphlet* Planet::find_graphlet(float x, float y) {
    IGraphlet* found = nullptr;

    if (this->head_graphlet != nullptr) {
		GraphletInfo* head_info = GRAPHLET_INFO(this->head_graphlet);
        IGraphlet* child = head_info->prev;

        do {
            GraphletInfo* info = GRAPHLET_INFO(child);

			if (unsafe_graphlet_unmasked(info, this->mode)) {
				float sx, sy, sw, sh;

				unsafe_fill_graphlet_bound(child, info, &sx, &sy, &sw, &sh);

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

IGraphlet* Planet::find_next_selected_graphlet(IGraphlet* start) {
	IGraphlet* found = nullptr;
	
	if (start == nullptr) {
		if (this->head_graphlet != nullptr) {
			found = do_search_selected_graphlet(this->head_graphlet, this->mode, this->head_graphlet);
		}
	} else {
		GraphletInfo* info = planet_graphlet_info(this, start);

		if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
			found = do_search_selected_graphlet(info->next, this->mode, this->head_graphlet);
		}
	}

	return found;
}

bool Planet::fill_graphlet_location(IGraphlet* g, float* x, float* y, GraphletAnchor a) {
	bool okay = false;
	GraphletInfo* info = planet_graphlet_info(this, g);
	
	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		float sx, sy, sw, sh, dx, dy;

		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		graphlet_anchor_offset(g, sw, sh, a, &dx, &dy);
		SET_BOX(x, sx + dx);
		SET_BOX(y, sy + dy);

		okay = true;
    }

	return okay;
}

bool Planet::fill_graphlet_boundary(IGraphlet* g, float* x, float* y, float* width, float* height) {
	bool okay = false;
	GraphletInfo* info = planet_graphlet_info(this, g);

	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		float sx, sy, sw, sh;
			
		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		SET_VALUES(x, sx, y, sy);
		SET_VALUES(width, sw, height, sh);

		okay = true;
	}

	return okay;
}

void Planet::fill_graphlets_boundary(float* x, float* y, float* width, float* height) {
    this->recalculate_graphlets_extent_when_invalid();
    SET_VALUES(x, this->graphlets_left, y, this->graphlets_top);
    SET_BOX(width, this->graphlets_right - this->graphlets_left);
    SET_BOX(height, this->graphlets_bottom - this->graphlets_top);
}

void Planet::size_cache_invalid() {
    this->graphlets_right = this->graphlets_left - 1.0F;
}

void Planet::recalculate_graphlets_extent_when_invalid() {
    if (this->graphlets_right < this->graphlets_left) {
        float rx, ry, width, height;

        if (this->head_graphlet == nullptr) {
            this->graphlets_left = 0.0F;
            this->graphlets_top = 0.0F;
            this->graphlets_right = 0.0F;
            this->graphlets_bottom = 0.0F;
        } else {
            IGraphlet* child = this->head_graphlet;

            this->graphlets_left = FLT_MAX;
            this->graphlets_top = FLT_MAX;
            this->graphlets_right = -FLT_MAX;
            this->graphlets_bottom = -FLT_MAX;

            do {
                GraphletInfo* info = GRAPHLET_INFO(child);

				if (unsafe_graphlet_unmasked(info, this->mode)) {
					unsafe_fill_graphlet_bound(child, info, &rx, &ry, &width, &height);
					this->graphlets_left = min(this->graphlets_left, rx);
					this->graphlets_top = min(this->graphlets_top, ry);
					this->graphlets_right = max(this->graphlets_right, rx + width);
					this->graphlets_bottom = max(this->graphlets_bottom, ry + height);
				}

                child = info->next;
            } while (child != this->head_graphlet);
        }

        this->info->master->min_width = max(this->graphlets_right, this->preferred_min_width);
        this->info->master->min_height = max(this->graphlets_bottom, this->preferred_min_height);
    }
}

void Planet::add_selected(IGraphlet* g) {
	if (this->rubberband_allowed) {
		GraphletInfo* info = planet_graphlet_info(this, g);

		if ((info != nullptr) && (!info->selected)) {
			if (unsafe_graphlet_unmasked(info, this->mode) && this->can_select(g)) {
				unsafe_add_selected(this, g, info);
			}
		}
	}
}

void Planet::set_selected(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

    if ((info != nullptr) && (!info->selected)) {
		if (unsafe_graphlet_unmasked(info, this->mode) && (this->can_select(g))) {
			unsafe_set_selected(this, g, info);
		}
    }
}

void Planet::no_selected() {
	if (this->head_graphlet != nullptr) {
		IGraphlet* child = this->head_graphlet;

		this->begin_update_sequence();

		do {
			GraphletInfo* info = GRAPHLET_INFO(child);

            if (info->selected && unsafe_graphlet_unmasked(info, this->mode)) {
				this->before_select(child, false);
				info->selected = false;
				this->after_select(child, false);
				this->notify_graphlet_updated(child);
			}

			child = info->next;
		} while (child != this->head_graphlet);

		this->end_update_sequence();
	}
}

IGraphlet* Planet::get_focus_graphlet() {
	return (this->graphlet_unmasked(this->focused_graphlet) ? this->focused_graphlet : nullptr);
}

void Planet::set_caret_owner(IGraphlet* g) {
	if (this->focused_graphlet != g) {
		if (g == nullptr) {
			this->focused_graphlet->own_caret(false);
			this->focused_graphlet = nullptr;
		} else if (g->handles_events()) {
			GraphletInfo* info = planet_graphlet_info(this, g);

			if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
				if (this->focused_graphlet != nullptr) {
					this->focused_graphlet->own_caret(false);
				}

				this->focused_graphlet = g;
				g->own_caret(true);
			}
		}
	}
}

/************************************************************************************************/
bool Planet::on_char(VirtualKey key) {
	bool handled = false;

	if (this->numpad->shown()) {
		handled = this->numpad->on_char(key);
	} else if (this->focused_graphlet != nullptr) {
		if (this->focused_graphlet->handles_events()) {
			handled = this->focused_graphlet->on_char(key);
		}
	}

	return handled;
}

void Planet::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
	if (g != nullptr) {
		GraphletInfo* info = GRAPHLET_INFO(g);

		if (this->can_select(g)) {
			if (!info->selected) {
				if (shifted) {
					if (this->rubberband_allowed) {
						unsafe_add_selected(this, g, info);
					}
				} else {
					unsafe_set_selected(this, g, info);
				}
			}
		} else {
			this->no_selected();
		}
	}
}

/************************************************************************************************/
bool Planet::on_pointer_pressed(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk, bool shifted, bool ctrled) {
	if (!this->numpad->is_colliding_with_mouse(x, y, keyboard_x, keyboard_y)) {
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

		this->numpad->show(false);

		this->set_caret_owner(unmasked_graphlet);
		if (unmasked_graphlet == nullptr) {
			this->no_selected();
		}

		switch (puk) {
		case PointerUpdateKind::LeftButtonPressed: {
			this->last_pointer_x = x;
			this->last_pointer_y = y;

			if (unmasked_graphlet == nullptr) {
				this->rubberband_x[0] = x;
				this->rubberband_x[1] = y;
				this->rubberband_y = this->rubberband_allowed ? (this->rubberband_x + 1) : nullptr;
			} else {
				this->rubberband_y = nullptr;

				if ((pdt == PointerDeviceType::Touch) && (this->hovering_graphlet != nullptr)) {
					GraphletInfo* info = GRAPHLET_INFO(this->hovering_graphlet);
					float local_x = x - info->x;
					float local_y = y - info->y;

					if (unmasked_graphlet->handles_events()) {
						unmasked_graphlet->on_hover(local_x, local_y, shifted, ctrled);
					}

					this->on_hover(unmasked_graphlet, local_x, local_y, shifted, ctrled);
				}
			}

			{ // Planet itself also has an opportunity to handle events directly.
				if (pdt == PointerDeviceType::Touch) {
					this->on_hover(nullptr, x, y, shifted, ctrled);
				}
			}
		} break;
		}
	}

	return true;
}

bool Planet::on_pointer_moved(float x, float y, VectorOfPointerPoint^ pps, PointerDeviceType pdt, PointerUpdateKind puk, bool shifted, bool ctrled) {
	bool handled = false;

	/** WARNING
	 * Touchscreen will never produce this event.
	 */

	if (puk == PointerUpdateKind::LeftButtonPressed) {
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
	} else {
		// NOTE non-left clicking always produces PointerUpdateKind::Other
		if (this->numpad->shown()) {
			float local_x = x - keyboard_x;
			float local_y = y - keyboard_y;

			if (this->numpad->is_colliding_with_mouse(x, y, keyboard_x, keyboard_y)) {
				this->numpad->on_hover(local_x, local_y, shifted, ctrled);
			} else {
				this->numpad->on_goodbye(local_x, local_y, shifted, ctrled);
			}
		} else {
			IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

			if (unmasked_graphlet != this->hovering_graphlet) {
				this->say_goodbye_to_the_hovering_graphlet(x, y, shifted, ctrled);
			}

			if (unmasked_graphlet != nullptr) {
				GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
				float local_x = x - info->x;
				float local_y = y - info->y;

				this->hovering_graphlet = unmasked_graphlet;

				if (unmasked_graphlet->handles_events()) {
					this->hovering_graphlet->on_hover(local_x, local_y, shifted, ctrled);
				}

				this->on_hover(unmasked_graphlet, local_x, local_y, shifted, ctrled);

				handled = true;
			}

			{ // Planet itself also has an opportunity to handle events directly.

				/** NOTE
				 * For PointerDeviceType other than ::Touch,
				 *  clients may have to produce the `on_goodbye` event on their own
				 *  if the PointerExited event handler is too rough.
				 */
				this->on_hover(nullptr, x, y, shifted, ctrled);
			}
		}
	}

	return handled;
}

bool Planet::on_pointer_released(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk, bool shifted, bool ctrled) {
	if (this->numpad->is_colliding_with_mouse(x, y, keyboard_x, keyboard_y)) {
		float local_x = x - keyboard_x;
		float local_y = y - keyboard_y;

		switch (puk) {
		case PointerUpdateKind::LeftButtonPressed: {
			this->numpad->on_tap(local_x, local_y, shifted, ctrled);

			if (pdt == PointerDeviceType::Touch) {
				this->numpad->on_goodbye(local_x, local_y, shifted, ctrled);
			}
		}; break;
		case PointerUpdateKind::RightButtonPressed: {
			this->numpad->on_right_tap(local_x, local_y, shifted, ctrled);
		}; break;
		}
	} else if (this->rubberband_y != nullptr) {
		this->rubberband_y = nullptr;
		// TODO: select all touched graphlets
	} else {
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

		if (unmasked_graphlet != nullptr) {
			GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
			float local_x = x - info->x;
			float local_y = y - info->y;

			switch (puk) {
			case PointerUpdateKind::LeftButtonPressed: {
				if (unmasked_graphlet->handles_events()) {
					unmasked_graphlet->on_tap(local_x, local_y, shifted, ctrled);

					if (pdt == PointerDeviceType::Touch) {
						unmasked_graphlet->on_goodbye(local_x, local_y, shifted, ctrled);
					}
				}

				this->on_tap(unmasked_graphlet, local_x, local_y, shifted, ctrled);

				if (pdt == PointerDeviceType::Touch) {
					this->on_goodbye(unmasked_graphlet, local_x, local_y, shifted, ctrled);
				}
			} break;
			case PointerUpdateKind::RightButtonPressed: {
				// NOTE: In macOS, Control + clicking produces a right clicking
				if (unmasked_graphlet->handles_events()) {
					unmasked_graphlet->on_right_tap(local_x, local_y, shifted, ctrled);
				}
				this->on_right_tap(unmasked_graphlet, local_x, local_y, shifted, ctrled);
			} break;
			}
		}

		{ // Planet itself also has an opportunity to handle events directly.
			switch (puk) {
			case PointerUpdateKind::LeftButtonPressed: {
				this->on_tap(nullptr, x, y, shifted, ctrled);
			} break;
			case PointerUpdateKind::RightButtonPressed: {
				// NOTE: In macOS, Control + clicking produces a right clicking
				this->on_right_tap(nullptr, x, y, shifted, ctrled);
			} break;
			}
		}
	}

	return true;
}

bool Planet::on_pointer_moveout(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk, bool shifted, bool ctrled) {
	return this->say_goodbye_to_the_hovering_graphlet(x, y, shifted, ctrled);
}

bool Planet::say_goodbye_to_the_hovering_graphlet(float x, float y, bool shifted, bool ctrled) {
	bool done = false;

	if (this->hovering_graphlet != nullptr) {
		GraphletInfo* info = GRAPHLET_INFO(this->hovering_graphlet);
		float local_x = x - info->x;
		float local_y = y - info->y;

		if (this->hovering_graphlet->handles_events()) {
			this->hovering_graphlet->on_goodbye(local_x, local_y, shifted, ctrled);
		}

		this->on_goodbye(this->hovering_graphlet, local_x, local_y, shifted, ctrled);

		this->hovering_graphlet = nullptr;
	}

	return done;
}

/*************************************************************************************************/
void Planet::show_virtual_keyboard(ScreenKeyboard type) {
	float auto_x, auto_y;

	this->numpad->fill_auto_position(&auto_x, &auto_y);
	this->show_virtual_keyboard(type, auto_x, auto_y);
}

void Planet::show_virtual_keyboard(ScreenKeyboard type, float x, float y) {
	this->numpad->show(true);
	this->keyboard_x = x;
	this->keyboard_y = y;
}

/*************************************************************************************************/
void Planet::append_decorator(IPlanetDecorator* decorator) {
	if (decorator != nullptr) {
		this->decorators.push_back(decorator);
		decorator->set_active_planet(this);
	}
}

void Planet::construct(CanvasCreateResourcesReason reason, float Width, float Height) {
	this->numpad->construct();
}

void Planet::update(long long count, long long interval, long long uptime) {
	if (this->numpad->shown()) {
		this->numpad->update(count, interval, uptime);
	}

	if (this->head_graphlet != nullptr) {
		IGraphlet* child = this->head_graphlet;

		do {
			GraphletInfo* info = GRAPHLET_INFO(child);

			if (unsafe_graphlet_unmasked(info, this->mode)) {
				child->update(count, interval, uptime);
			}
			
			child = info->next;
		} while (child != this->head_graphlet);
    }

	for (IPlanetDecorator* decorator : this->decorators) {
		decorator->update(count, interval, uptime);
	}

	this->on_elapse(count, interval, uptime);
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

	for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
		try {
#endif
			decorator->draw_before(ds, Width, Height);
#ifdef _DEBUG
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Critical, L"%s: predecorating: %s", this->name()->Data(), e->Message->Data());
		}
#endif
	}

	if (this->head_graphlet != nullptr) {
		IGraphlet* child = this->head_graphlet;
		float width, height;

		do {
			GraphletInfo* info = GRAPHLET_INFO(child);

			if (unsafe_graphlet_unmasked(info, this->mode)) {
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

					for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
						try {
#endif
							decorator->draw_before_graphlet(child, ds, info->x, info->y, width, height, info->selected);
#ifdef _DEBUG
						} catch (Platform::Exception^ e) {
							this->get_logger()->log_message(Log::Critical, L"%s: predecorating graphlet: %s",
								this->name()->Data(), e->Message->Data());
						}
#endif
					}

#ifdef _DEBUG
					try {
#endif
						if (child->ready()) {
							child->draw(ds, info->x, info->y, width, height);
						} else {
							child->draw_progress(ds, info->x, info->y, width, height);
						}
#ifdef _DEBUG
					} catch (Platform::Exception^ e) {
						this->get_logger()->log_message(Log::Critical, L"%s: rendering graphlet: %s",
							this->name()->Data(), e->Message->Data());
					}
#endif	

					for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
						try {
#endif
							decorator->draw_after_graphlet(child, ds, info->x, info->y, width, height, info->selected);
#ifdef _DEBUG
						} catch (Platform::Exception^ e) {
							this->get_logger()->log_message(Log::Critical, L"%s: postdecorating graphlet: %s",
								this->name()->Data(), e->Message->Data());
						}
#endif
					}

					if (info->selected) {
						this->draw_visible_selection(ds, info->x, info->y, width, height);
					}

					delete layer; // Must Close the Layer Explicitly, it is C++/CX's quirk.
					ds->Transform = transform;
				}
			}

			child = info->next;
		} while (child != this->head_graphlet);
	}

	if (this->rubberband_y != nullptr) {
		ICanvasBrush^ rubberband_color = Colours::Highlight;

		float left = min(this->last_pointer_x, (*this->rubberband_x));
		float top = min(this->last_pointer_y, (*this->rubberband_y));
		float width = abs((*this->rubberband_x) - this->last_pointer_x);
		float height = abs((*this->rubberband_y) - this->last_pointer_y);

		rubberband_color->Opacity = 0.32F;
		ds->FillRectangle(left, top, width, height, rubberband_color);
		rubberband_color->Opacity = 1.00F;
		ds->DrawRectangle(left, top, width, height, rubberband_color);
	}

	for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
		try {
#endif
			decorator->draw_after(ds, Width, Height);
#ifdef _DEBUG
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Critical, L"%s: postdecorating: %s", this->name()->Data(), e->Message->Data());
		}
#endif
	}

	if (this->numpad->shown()) {
		float width, height;

		this->numpad->fill_extent(0.0F, 0.0F, &width, &height);
		this->numpad->draw(ds, this->keyboard_x, this->keyboard_y, width, height);
	}
}

void Planet::draw_visible_selection(CanvasDrawingSession^ ds, float x, float y, float width, float height) {
	static CanvasStrokeStyle^ dash = make_dash_stroke(CanvasDashStyle::Dash);

	ds->DrawRectangle(x, y, width, height, Colours::Highlight, 1.0F, dash);
}

/*************************************************************************************************/
IPlanet::IPlanet(Platform::String^ name) : caption(name) {}

IPlanet::~IPlanet() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
}

Platform::String^ IPlanet::name() {
	return this->caption;
}

IDisplay^ IPlanet::master() {
	IDisplay^ display = nullptr;

	if (this->info != nullptr) {
		display = this->info->master;
	}

	return display;
}

Platform::Object^ IPlanet::navigation_label() {
	return speak(this->caption);
}

bool IPlanet::shown() {
	return (this->info != nullptr) && (this->info->master->shown());
}

bool IPlanet::surface_ready() {
	bool ready = false;

	if (this->info != nullptr) {
		ready = this->info->master->surface_ready();
	}

	return ready;
}

bool IPlanet::ui_thread_ready() {
	bool ready = false;

	if (this->info != nullptr) {
		ready = this->info->master->ui_thread_ready();
	}

	return ready;
}

float IPlanet::actual_width() {
	float width = 0.0F;

	if (this->info != nullptr) {
		width = this->info->master->actual_width;
	}

	return width;
}

float IPlanet::actual_height() {
	float height = 0.0F;

	if (this->info != nullptr) {
		height = this->info->master->actual_height;
	}

	return height;
}

float IPlanet::sketch_to_application_width(float sketch_width) {
	float width = sketch_width;

	if (this->info != nullptr) {
		width = this->info->master->sketch_to_application_width(sketch_width);
	}

	return width;
}

float IPlanet::sketch_to_application_height(float sketch_height) {
	float height = sketch_height;

	if (this->info != nullptr) {
		height = this->info->master->sketch_to_application_height(sketch_height);
	}

	return height;
}

Syslog* IPlanet::get_logger() {
	Syslog* logger = default_logger();

	if (this->info != nullptr) {
		logger = this->info->master->get_logger();
	}

	return logger;
}

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

void IPlanet::collapse() {
	this->erase();
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
			syslog(Log::Alarm, "failed to save universe as bitmap:" + e->Message);
		}
	});
}

Point IPlanet::global_to_local_point(IGraphlet* g, float global_x, float global_y, float xoff, float yoff) {
	float gx, gy;

	this->fill_graphlet_location(g, &gx, &gy);

	return Point(global_x - gx + xoff, global_y - gy + yoff);
}

Point IPlanet::local_to_global_point(IGraphlet* g, float local_x, float local_y, float xoff, float yoff) {
	float gx, gy;

	this->fill_graphlet_location(g, &gx, &gy);
	
	return Point(gx + local_x + xoff, gy + local_y + yoff);
}
