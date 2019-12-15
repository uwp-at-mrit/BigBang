#include <ppltasks.h>
#include <WindowsNumerics.h>

#include "datum/path.hpp"
#include "datum/flonum.hpp"

#include "planet.hpp"
#include "syslog.hpp"
#include "system.hpp"
#include "tongue.hpp"

#include "math.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "brushes.hxx"
#include "transformation.hpp"

#include "graphlet/primitive.hpp"
#include "decorator/decorator.hpp"

#include "virtualization/keyboard/numpad.hpp"
#include "virtualization/keyboard/affinepad.hpp"
#include "virtualization/keyboard/bucketpad.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Windows::ApplicationModel;
using namespace Windows::Devices::Input;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

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

namespace {
	private struct AsyncInfo {
		float x0;
		float y0;
		float fx0;
		float fy0;
		float dx0;
		float dy0;
	};

	private class GraphletInfo : public WarGrey::SCADA::IGraphletInfo {
	public:
		GraphletInfo(IPlanet* master, unsigned int mode)
			: IGraphletInfo(master), mode(mode), alpha(1.0F), async(nullptr) {};

	public:
		float x;
		float y;
		float alpha;
		float rotation;
		bool selected;

	public:
		unsigned int mode;

	public: // for asynchronously loaded graphlets
		AsyncInfo* async;

	public:
		IGraphlet* next;
		IGraphlet* prev;
	};
}

static inline GraphletInfo* bind_graphlet_owership(IPlanet* master, unsigned int mode, IGraphlet* g) {
    auto info = new GraphletInfo(master, mode);
    
	g->info = info;

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
		// WARNING: rotation is unimplemented yet, it is recommanded that, graphlet do rotating on its own.
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

static void graphlet_anchor_fraction(GraphletAnchor& a, float* ofx, float* ofy) {
	float fx = 0.0F;
	float fy = 0.0F;

	if (a != GraphletAnchor::LT) {
		switch (a) {
		case GraphletAnchor::LC:            fy = 0.5F; break;
		case GraphletAnchor::LB:            fy = 1.0F; break;
		case GraphletAnchor::CT: fx = 0.5F;            break;
		case GraphletAnchor::CC: fx = 0.5F; fy = 0.5F; break;
		case GraphletAnchor::CB: fx = 0.5F; fy = 1.0F; break;
		case GraphletAnchor::RT: fx = 1.0F;            break;
		case GraphletAnchor::RC: fx = 1.0F; fy = 0.5F; break;
		case GraphletAnchor::RB: fx = 1.0F; fy = 1.0F; break;
		}
	}

	(*ofx) = fx;
	(*ofy) = fy;
}

static inline void graphlet_anchor_offset(float width, float height, GraphletAnchor& a, float* xoff, float* yoff) {
    float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	(*xoff) = fx * width;
	(*yoff) = fy * height;
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

static bool unsafe_move_graphlet_via_info(Planet* master, IGraphlet* g, GraphletInfo* info
	, float x, float y, float fx, float fy, float dx, float dy, bool absolute) {
	float sx, sy, sw, sh;
	float ax = 0.0F;
	float ay = 0.0F;
	
	if (g->ready()) {
		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		ax = (sw * fx);
		ay = (sh * fy);
	} else {
		info->async = new AsyncInfo();

		info->async->x0 = x;
		info->async->y0 = y;
		info->async->fx0 = fx;
		info->async->fy0 = fy;
		info->async->dx0 = dx;
		info->async->dy0 = dy;
	}
	
	return unsafe_move_graphlet_via_info(master, info, x - ax + dx, y - ay + dy, true);
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

static void do_resize(Planet* master, IGraphlet* g, GraphletInfo* info, float scale_x, float scale_y, float prev_scale_x = 1.0F, float prev_scale_y = 1.0F) {
	GraphletAnchor resize_anchor;

	// TODO: the theory or implementation seems incorrect. 

	if (g->resizable(&resize_anchor)) {
		float sx, sy, sw, sh, fx, fy, nx, ny, nw, nh;

		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		graphlet_anchor_fraction(resize_anchor, &fx, &fy);

		g->resize((sw / prev_scale_x) * scale_x, (sh / prev_scale_y) * scale_y);
		g->fill_extent(sx, sy, &nw, &nh);

		nx = sx + (sw - nw) * fx;
		ny = sy + (sh - nh) * fy;

		unsafe_move_graphlet_via_info(master, info, nx, ny, true);
	}
}

static Size monitor(0.0F, 0.0F);

/*************************************************************************************************/
Planet::Planet(Platform::String^ name, unsigned int initial_mode)
	: IPlanet(name), mode(initial_mode), background(nullptr), translate_x(0.0F), translate_y(0.0F), scale_x(1.0F), scale_y(1.0F) {
	this->numpad = new Numpad(this);
	this->arrowpad = new Affinepad(this);
	this->bucketpad = new Bucketpad(this);

	this->keyboard = this->numpad;

	if (monitor.Width == 0.0F) {
		monitor = system_screen_size();
	}
}

Planet::~Planet() {
	this->collapse();
    
	for (IPlanetDecorator* decorator : this->decorators) {
		delete decorator;
	}

	delete this->numpad;
	delete this->arrowpad;
	delete this->bucketpad;
}

void Planet::change_mode(unsigned int mode) {
	if (mode != this->mode) {
		this->no_selected();
		this->keyboard->show(false);
		this->mode = mode;
		this->size_cache_invalid();
		this->notify_graphlet_updated(nullptr);
	}
}

unsigned int Planet::current_mode() {
	return this->mode;
}

bool Planet::graphlet_unmasked(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	return ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode));
}

void Planet::notify_graphlet_ready(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	if (info != nullptr) {
		if (info->async != nullptr) {
			this->size_cache_invalid();
			this->begin_update_sequence();

			unsafe_move_graphlet_via_info(this, g, info,
				info->async->x0, info->async->y0, info->async->fx0, info->async->fy0, info->async->dx0, info->async->dy0,
				true);

			if ((this->scale_x != 1.0F) || (this->scale_y != 1.0F)) {
				do_resize(this, g, info, this->scale_x, this->scale_y);
			}

			delete info->async;
			info->async = nullptr;

			this->notify_graphlet_updated(g);
			this->on_graphlet_ready(g);
			this->end_update_sequence();
		}
	}
}

void Planet::insert(IGraphlet* g, float x, float y, float fx, float fy, float dx, float dy) {
	if (g->info == nullptr) {
		GraphletInfo* info = bind_graphlet_owership(this, this->mode, g);

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

		this->begin_update_sequence();
		g->sprite();
		g->construct();
		g->sprite_construct();
		unsafe_move_graphlet_via_info(this, g, info, x, y, fx, fy, dx, dy, true);

		if (g->ready()) {
			if ((this->scale_x != 1.0F) || (this->scale_y != 1.0F)) {
				do_resize(this, g, info, this->scale_x, this->scale_y);
			}

			this->notify_graphlet_updated(g);
			this->on_graphlet_ready(g);
			this->end_update_sequence();
		} else {
			this->notify_graphlet_updated(g); // is it necessary?
			this->end_update_sequence();
		}

#if _DEBUG
	} else {
		this->get_logger()->log_message(Log::Warning,
			L"%s: inserting: ignored graphlet since it has already had a owner",
			this->name()->Data());
#endif
	}
}

void Planet::insert(IGraphlet* g, IGraphlet* target, float tfx, float tfy, float fx, float fy, float dx, float dy) {
	if (g->info == nullptr) {
		GraphletInfo* tinfo = planet_graphlet_info(this, target);
		float x = 0.0F;
		float y = 0.0F;

		// TODO: what if the target graphlet is not ready?

		if ((tinfo != nullptr) && unsafe_graphlet_unmasked(tinfo, this->mode)) {
			float tsx, tsy, tsw, tsh;

			unsafe_fill_graphlet_bound(target, tinfo, &tsx, &tsy, &tsw, &tsh);
			x = tsx + tsw * tfx;
			y = tsy + tsh * tfy;
		}

		this->insert(g, x, y, fx, fy, dx, dy);
#ifdef _DEBUG
	} else {
		this->get_logger()->log_message(Log::Warning,
			L"%s: inserting: ignored graphlet since it has already had a owner",
			this->name()->Data());
#endif
	}
}

void Planet::insert(IGraphlet* g, IGraphlet* xtarget, float xfx, IGraphlet* ytarget, float yfy, float fx, float fy, float dx, float dy) {
	if (g->info == nullptr) {
		GraphletInfo* xinfo = planet_graphlet_info(this, xtarget);
		GraphletInfo* yinfo = planet_graphlet_info(this, ytarget);
		float x = 0.0F;
		float y = 0.0F;

		// TODO: what if the target graphlet is not ready?

		if ((xinfo != nullptr) && unsafe_graphlet_unmasked(xinfo, this->mode)
			&& (yinfo != nullptr) && unsafe_graphlet_unmasked(yinfo, this->mode)) {
			float xsx, xsy, xsw, xsh, ysx, ysy, ysw, ysh;

			unsafe_fill_graphlet_bound(xtarget, xinfo, &xsx, &xsy, &xsw, &xsh);
			unsafe_fill_graphlet_bound(ytarget, yinfo, &ysx, &ysy, &ysw, &ysh);
			x = xsx + xsw * xfx;
			y = ysy + ysh * yfy;
		}

		this->insert(g, x, y, fx, fy, dx, dy);
#ifdef _DEBUG
	} else {
		this->get_logger()->log_message(Log::Warning,
			L"%s: inserting: ignored graphlet since it has already had a owner",
			this->name()->Data());
#endif
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

void Planet::move_to(IGraphlet* g, float x, float y, float fx, float fy, float dx, float dy) {
	GraphletInfo* info = planet_graphlet_info(this, g);
	
	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		if (unsafe_move_graphlet_via_info(this, g, info, x, y, fx, fy, dx, dy, true)) {
			this->notify_graphlet_updated(g);
		}
	}
}

void Planet::move_to(IGraphlet* g, IGraphlet* target, float tfx, float tfy, float fx, float fy, float dx, float dy) {
	GraphletInfo* tinfo = planet_graphlet_info(this, target);
	float x = 0.0F;
	float y = 0.0F;

	if ((tinfo != nullptr) && unsafe_graphlet_unmasked(tinfo, this->mode)) {
		float tsx, tsy, tsw, tsh;

		unsafe_fill_graphlet_bound(target, tinfo, &tsx, &tsy, &tsw, &tsh);
		x = tsx + tsw * tfx;
		y = tsy + tsh * tfy;
	}
		
	this->move_to(g, x, y, fx, fy, dx, dy);
}

void Planet::move_to(IGraphlet* g, IGraphlet* xtarget, float xfx, IGraphlet* ytarget, float yfy, float fx, float fy, float dx, float dy) {
	GraphletInfo* xinfo = planet_graphlet_info(this, xtarget);
	GraphletInfo* yinfo = planet_graphlet_info(this, ytarget);
	float x = 0.0F;
	float y = 0.0F;

	if ((xinfo != nullptr) && unsafe_graphlet_unmasked(xinfo, this->mode)
		&& (yinfo != nullptr) && unsafe_graphlet_unmasked(yinfo, this->mode)) {
		float xsx, xsy, xsw, xsh, ysx, ysy, ysw, ysh;

		unsafe_fill_graphlet_bound(xtarget, xinfo, &xsx, &xsy, &xsw, &xsh);
		unsafe_fill_graphlet_bound(ytarget, yinfo, &ysx, &ysy, &ysw, &ysh);
		x = xsx + xsw * xfx;
		y = ysy + ysh * yfy;
	}

	this->move_to(g, x, y, fx, fy, dx, dy);
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

void Planet::translate(float x, float y) {
	if ((this->translate_x != x) || (this->translate_y != y)) {
		this->translate_x = x;
		this->translate_y = y;
		this->notify_graphlet_updated(nullptr);
	}
}

void Planet::scale(float xscale, float yscale) {
	// TODO: implement flipping
	if (xscale > 0.0F) {
		if (yscale <= 0.0F) {
			yscale = xscale;
		}

		if ((this->scale_x != xscale) || (this->scale_y != yscale)) {
			this->begin_update_sequence();

			if (this->head_graphlet != nullptr) {
				GraphletInfo* head_info = GRAPHLET_INFO(this->head_graphlet);
				IGraphlet* child = head_info->prev;
				
				do {
					GraphletInfo* info = GRAPHLET_INFO(child);
					
					if (unsafe_graphlet_unmasked(info, this->mode)) {
						do_resize(this, child, info, xscale, yscale, this->scale_x, this->scale_y);
					}

					child = info->prev;
				} while (child != head_info->prev);
			}

			this->scale_x = xscale;
			this->scale_y = yscale;

			if (this->needs_update()) {
				this->size_cache_invalid();
			}

			this->end_update_sequence();
		}
	}
}

void Planet::set_background(ICanvasBrush^ background, float corner_radius) {
	this->background = background;
	this->background_corner_radius = corner_radius;
}

void Planet::cellophane(IGraphlet* g, float opacity) {
	GraphletInfo* info = planet_graphlet_info(this, g);

	if (info != nullptr) {
		info->alpha = opacity;
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
				if (!child->concealled()) {
					float sx, sy, sw, sh;

					unsafe_fill_graphlet_bound(child, info, &sx, &sy, &sw, &sh);

					sx += (this->translate_x * this->scale_x);
					sy += (this->translate_y * this->scale_y);

					if ((sx < x) && (x < (sx + sw)) && (sy < y) && (y < (sy + sh))) {
						if (child->is_colliding_with_mouse(x - sx, y - sy)) {
							found = child;
							break;
						}
					}
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

IGraphlet* Planet::thumbnail_graphlet() {
	return nullptr;
}

bool Planet::fill_graphlet_location(IGraphlet* g, float* x, float* y, float fx, float fy) {
	bool okay = false;
	GraphletInfo* info = planet_graphlet_info(this, g);
	
	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		float sx, sy, sw, sh;

		unsafe_fill_graphlet_bound(g, info, &sx, &sy, &sw, &sh);
		SET_BOX(x, sx + sw * fx);
		SET_BOX(y, sy + sh * fy);

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

            this->graphlets_left = infinity_f;
            this->graphlets_top = infinity_f;
            this->graphlets_right = -infinity_f;
            this->graphlets_bottom = -infinity_f;

            do {
                GraphletInfo* info = GRAPHLET_INFO(child);

				if (unsafe_graphlet_unmasked(info, this->mode)) {
					unsafe_fill_graphlet_bound(child, info, &rx, &ry, &width, &height);
					this->graphlets_left = flmin(this->graphlets_left, rx);
					this->graphlets_top = flmin(this->graphlets_top, ry);
					this->graphlets_right = flmax(this->graphlets_right, rx + width);
					this->graphlets_bottom = flmax(this->graphlets_bottom, ry + height);
				}

                child = info->next;
            } while (child != this->head_graphlet);
        }

        this->info->master->min_resize(this->graphlets_right, this->graphlets_bottom);
    }
}

void Planet::add_selected(IGraphlet* g) {
	if (this->can_select_multiple()) {
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

bool Planet::is_selected(IGraphlet* g) {
	GraphletInfo* info = planet_graphlet_info(this, g);
	bool selected = false;

	if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
		selected = info->selected;
	}

	return selected;
}

unsigned int Planet::count_selected() {
	unsigned int n = 0U;

	if (this->head_graphlet != nullptr) {
		IGraphlet* child = this->head_graphlet;

		do {
			GraphletInfo* info = GRAPHLET_INFO(child);

			if (info->selected && unsafe_graphlet_unmasked(info, this->mode)) {
				n += 1U;
			}

			child = info->next;
		} while (child != this->head_graphlet);
	}

	return n;
}

IGraphlet* Planet::get_focus_graphlet() {
	return (this->graphlet_unmasked(this->focused_graphlet) ? this->focused_graphlet : nullptr);
}

void Planet::set_caret_owner(IGraphlet* g) {
	if (this->focused_graphlet != g) {
		if ((g != nullptr) && (g->handle_events())) {
			GraphletInfo* info = planet_graphlet_info(this, g);

			if ((info != nullptr) && unsafe_graphlet_unmasked(info, this->mode)) {
				if (this->focused_graphlet != nullptr) {
					this->focused_graphlet->own_caret(false);
					this->on_focus(this->focused_graphlet, false);
				}

				this->focused_graphlet = g;
				g->own_caret(true);

				this->on_focus(g, true);
			}
		} else if (this->focused_graphlet != nullptr) {
			this->focused_graphlet->own_caret(false);
			this->on_focus(this->focused_graphlet, false);
			this->focused_graphlet = nullptr;
		}
	} else if (g != nullptr) {
		this->on_focus(g, true);
	}
}

/************************************************************************************************/
bool Planet::on_key(VirtualKey key, bool wargrey_keyboard) {
	bool handled = false;

	if (this->keyboard->shown()) {
		this->keyboard->on_key(key, wargrey_keyboard);
	}
	
	if (this->focused_graphlet != nullptr) {
		handled = this->focused_graphlet->on_key(key, wargrey_keyboard);
	}

	return handled;
}

bool Planet::on_character(unsigned int keycode) {
	bool handled = false;

	if (this->keyboard->shown()) {
		handled = this->keyboard->on_character(keycode);
	}

	if ((!handled) && (this->focused_graphlet != nullptr)) {
		handled = this->focused_graphlet->on_character(keycode);
	}

	return handled;
}

void Planet::on_swipe(IGraphlet* g, float local_x, float local_y) {
	GraphletInfo* info = GRAPHLET_INFO(g);

	if (!info->selected) {
		if (this->can_select(g)) {
			unsafe_add_selected(this, g, info);
		}
	}
}

void Planet::on_tap(IGraphlet* g, float local_x, float local_y) {
	if (g != nullptr) {
		GraphletInfo* info = GRAPHLET_INFO(g);

		if (!info->selected) {
			if (this->can_select(g)) {
				unsafe_set_selected(this, g, info);

				if (g->handle_events()) {
					this->set_caret_owner(g);
				}
			} else {
				this->no_selected();
			}
		}
	}
}

/************************************************************************************************/
bool Planet::on_pointer_pressed(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) {
	bool pressed_on_keyboard = this->keyboard->is_colliding_with_mouse(x, y, this->keyboard_x, this->keyboard_y);
	bool handled = false;

	if (pressed_on_keyboard && (pdt == PointerDeviceType::Touch)) {
		float local_x = x - this->keyboard_x;
		float local_y = y - this->keyboard_y;

		this->keyboard->on_hover(local_x, local_y);

		handled = true;
	} else if (!pressed_on_keyboard) {
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

		this->keyboard->show(false);
		this->set_caret_owner(unmasked_graphlet);
		this->no_selected();

		switch (puk) {
		case PointerUpdateKind::LeftButtonPressed: {
			this->track_thickness = 2.0F;

#ifdef _DEBUG
			this->figure_track = nullptr;
#endif

			this->figure_anchors.clear();

			if (this->can_select_multiple()) {
				this->figure_anchors.push_back(float2(x, y));
			}

			if (pdt == PointerDeviceType::Touch) {
				this->track_thickness = 8.0F;

				if (unmasked_graphlet != this->hovering_graphlet) {
					this->say_goodbye_to_hover_graphlet(x, y, pdt, puk);
				}

				if (unmasked_graphlet != nullptr) {
					GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
					float local_x = x - info->x;
					float local_y = y - info->y;

					this->hovering_graphlet = unmasked_graphlet;

					if (this->hovering_graphlet->handle_events()) {
						this->hovering_graphlet->on_hover(local_x, local_y);

						if (this->hovering_graphlet->handle_low_level_events()) {
							this->hovering_graphlet->on_pointer_pressed(local_x, local_y, pdt, puk);
						}
					}

					this->on_hover(this->hovering_graphlet, local_x, local_y);

					handled = true;
				}
			} else {
				if ((unmasked_graphlet != nullptr) && (unmasked_graphlet->handle_low_level_events())) {
					GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
					float local_x = x - info->x;
					float local_y = y - info->y;

					unmasked_graphlet->on_pointer_pressed(local_x, local_y, pdt, puk);
				}
			}

			{ // Planet itself also has an opportunity to handle events directly.
				if (pdt == PointerDeviceType::Touch) {
					this->on_hover(nullptr, x, y);
				}
			}
		} break;
		}
	}

	return handled;
}

bool Planet::on_pointer_moved(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) {
	bool handled = false;

	if (!this->figure_anchors.empty()) {
		float2 last_anchor = this->figure_anchors.back();
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

		if (unmasked_graphlet != nullptr) {
			if (unmasked_graphlet != nullptr) {
				GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
				float local_x = x - info->x;
				float local_y = y - info->y;

				this->on_swipe(unmasked_graphlet, local_x, local_y);
			}
		}

#ifdef _DEBUG
		auto segment = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

		segment->BeginFigure(last_anchor.x, last_anchor.y);
		segment->AddLine(x, y);
		segment->EndFigure(CanvasFigureLoop::Open);

		if (this->figure_track == nullptr) {
			this->figure_track = CanvasGeometry::CreatePath(segment);
		} else {
			this->figure_track = geometry_union(this->figure_track, CanvasGeometry::CreatePath(segment));
		}
#endif

		if ((x != last_anchor.x) || (y != last_anchor.y)) {
			this->figure_anchors.push_back(float2(x, y));
		}
	}

	if (puk != PointerUpdateKind::LeftButtonPressed) {
		// NOTE non-left clicking always produces PointerUpdateKind::Other
		if (this->keyboard->shown()) {
			float local_x = x - this->keyboard_x;
			float local_y = y - this->keyboard_y;

			if (this->keyboard->is_colliding_with_mouse(x, y, this->keyboard_x, this->keyboard_y)) {
				this->keyboard->on_hover(local_x, local_y);

				handled = true;
			} else {
				this->keyboard->on_goodbye(local_x, local_y);
			}
		} else {
			IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

			if (unmasked_graphlet != this->hovering_graphlet) {
				this->say_goodbye_to_hover_graphlet(x, y, pdt, puk);
			}

			if (unmasked_graphlet != nullptr) {
				GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
				float local_x = x - info->x;
				float local_y = y - info->y;

				this->hovering_graphlet = unmasked_graphlet;

				if (unmasked_graphlet->handle_events()) {
					unmasked_graphlet->on_hover(local_x, local_y);

					if (unmasked_graphlet->handle_low_level_events()) {
						unmasked_graphlet->on_pointer_moved(local_x, local_y, pdt, puk);
					}
				}

				this->on_hover(this->hovering_graphlet, local_x, local_y);

				handled = true;
			}

			{ // Planet itself also has an opportunity to handle events directly.

				/** NOTE
				 * For PointerDeviceType other than ::Touch,
				 *  clients may have to produce the `on_goodbye` event on their own
				 *  if the PointerExited event handler is too rough.
				 */
				this->on_hover(nullptr, x, y);
			}
		}
	}

	return handled;
}

bool Planet::on_pointer_released(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) {
	bool handled = false;

	/** NOTE
	 * The `*Pressed` kinds of updating are used for tolerating, they are more intuitive.
	 *
	 *  The actual kinds provided by pointer devices are `*Released`,
	 *  but our API may not follow the devices.
	 */

	if (this->keyboard->is_colliding_with_mouse(x, y, keyboard_x, keyboard_y)) {
		float local_x = x - keyboard_x;
		float local_y = y - keyboard_y;

		switch (puk) {
		case PointerUpdateKind::LeftButtonReleased:
		case PointerUpdateKind::LeftButtonPressed: {
			this->keyboard->on_tap(local_x, local_y);

			if (pdt == PointerDeviceType::Touch) {
				this->keyboard->on_goodbye(local_x, local_y);
			}

			handled = true;
		}; break;
		}
	} else {
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);
		size_t anchor_count = this->figure_anchors.size();
		
		if (anchor_count <= 8) {
			if (unmasked_graphlet != nullptr) {
				GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
				float local_x = x - info->x;
				float local_y = y - info->y;

				switch (puk) {
				case PointerUpdateKind::LeftButtonReleased:
				case PointerUpdateKind::LeftButtonPressed: {
					if (unmasked_graphlet->handle_events()) {
						unmasked_graphlet->on_tap(local_x, local_y);

						if (pdt == PointerDeviceType::Touch) {
							unmasked_graphlet->on_goodbye(local_x, local_y);
						}

						if (unmasked_graphlet->handle_low_level_events()) {
							unmasked_graphlet->on_pointer_released(local_x, local_y, pdt, puk);
						}
					}

					this->on_tap(unmasked_graphlet, local_x, local_y);

					if (info->selected) {
						this->on_tap_selected(unmasked_graphlet, local_x, local_y);
					}

					if (pdt == PointerDeviceType::Touch) {
						this->on_goodbye(unmasked_graphlet, local_x, local_y);
					}

					handled = info->selected;
				}; break;
				}
			}

			{ // Planet itself also has an opportunity to handle events directly.
				switch (puk) {
				case PointerUpdateKind::LeftButtonReleased:
				case PointerUpdateKind::LeftButtonPressed: {
					this->on_tap(nullptr, x, y);
				} break;
				}
			}
		} else {
			this->on_gesture(this->figure_anchors, x, y);

			handled = true;
		}

#ifdef _DEBUG
		if (this->figure_anchors.size() > 8U) {
			if (dynamic_cast<IHeadUpPlanet*>(this) == nullptr) {
				this->save(ms_apptemp_file(this->name(), ".png"), this->actual_width(), this->actual_height());
			}
		} else {
			this->figure_track = nullptr;
		}
#endif
	}

	return handled;
}

bool Planet::on_pointer_wheeled(float x, float y, float delta0, bool horizontal, bool controlled) {
	bool handled = false;

	if (!this->keyboard->is_colliding_with_mouse(x, y, keyboard_x, keyboard_y)) {
		IGraphlet* unmasked_graphlet = this->find_graphlet(x, y);

		if ((unmasked_graphlet != nullptr) && (this->focused_graphlet == unmasked_graphlet) && (unmasked_graphlet->handle_events())) {
			GraphletInfo* info = GRAPHLET_INFO(unmasked_graphlet);
			float local_x = x - info->x;
			float local_y = y - info->y;
			float delta = delta0;

			if (controlled || (!unmasked_graphlet->handle_wheel_translation())) {
				float zoom_magic_base = 1.0618F;

				if (delta > 0.0F) {
					delta = flexpt(zoom_magic_base, delta);
				} else {
					delta = flexpt(1.0F / zoom_magic_base, -delta);
				}

				handled = unmasked_graphlet->on_wheel_zoom(local_x, local_y, delta);
			} else {
				float translation_magic_base = 16.0F;

				delta *= translation_magic_base;

				if (!horizontal) {
					delta *= -1.0F;
				}

				handled = unmasked_graphlet->on_wheel_translation(local_x, local_y, delta, horizontal);
			}

			if ((!handled) && unmasked_graphlet->handle_low_level_events()) {
				handled = unmasked_graphlet->on_pointer_wheeled(local_x, local_y, delta0, horizontal, controlled);
			}
		}
	}

	return handled;
}

bool Planet::on_pointer_moveout(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) {
	return this->say_goodbye_to_hover_graphlet(x, y, pdt, puk);
}

bool Planet::say_goodbye_to_hover_graphlet(float x, float y, PointerDeviceType pdt, PointerUpdateKind puk) {
	bool done = false;

	if (this->hovering_graphlet != nullptr) {
		GraphletInfo* info = GRAPHLET_INFO(this->hovering_graphlet);
		float local_x = x - info->x;
		float local_y = y - info->y;

		if (this->hovering_graphlet->handle_events()) {
			this->hovering_graphlet->on_goodbye(local_x, local_y);

			if (this->hovering_graphlet->handle_low_level_events()) {
				this->hovering_graphlet->on_pointer_moveout(local_x, local_y, pdt, puk);
			}
		}

		this->on_goodbye(this->hovering_graphlet, local_x, local_y);

		this->hovering_graphlet = nullptr;
	}

	return done;
}

/*************************************************************************************************/
void Planet::show_virtual_keyboard(ScreenKeyboard type, float x, float y, float dx, float dy) {
	this->switch_virtual_keyboard(type);

	this->keyboard->show(true);
	this->keyboard_x = x + dx;
	this->keyboard_y = y + dy;
}

void Planet::show_virtual_keyboard(ScreenKeyboard type, IGraphlet* g, GraphletAnchor a, float dx, float dy) {
	float auto_x, auto_y;

	this->switch_virtual_keyboard(type);
	this->keyboard->fill_auto_position(&auto_x, &auto_y, g, a);
	this->show_virtual_keyboard(type, auto_x, auto_y, dx, dy);
}

void Planet::show_virtual_keyboard(ScreenKeyboard type, GraphletAnchor a, float dx, float dy) {
	this->show_virtual_keyboard(type, this->get_focus_graphlet(), a, dx, dy);
}

void Planet::hide_virtual_keyboard() {
	if (this->keyboard != nullptr) {
		this->keyboard->show(false);
	}
}

void Planet::switch_virtual_keyboard(ScreenKeyboard type) {
	switch (type) {
	case ScreenKeyboard::Numpad:   this->keyboard = this->numpad; break;
	case ScreenKeyboard::Affinepad: this->keyboard = this->arrowpad; break;
	case ScreenKeyboard::Bucketpad:  this->keyboard = this->bucketpad; break;
	}
}

/*************************************************************************************************/
void Planet::push_decorator(IPlanetDecorator* decorator) {
	if (decorator != nullptr) {
		this->decorators.push_back(decorator);
		decorator->set_active_planet(this);
	}
}

void Planet::construct(CanvasCreateResourcesReason reason, float Width, float Height) {
	IKeyboard* keyboards[] = { this->numpad, this->arrowpad, this->bucketpad };

	for (unsigned int idx = 0; idx < (sizeof(keyboards) / sizeof(Keyboard*)); idx++) {
		keyboards[idx]->sprite();
		keyboards[idx]->construct();
		keyboards[idx]->sprite_construct();
	}
}

void Planet::on_elapse(long long count, long long interval, long long uptime) {
	if (this->keyboard->shown()) {
		this->keyboard->update(count, interval, uptime);
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

	this->update(count, interval, uptime);
}

void Planet::draw(CanvasDrawingSession^ ds, float X, float Y, float Width, float Height) {
	CanvasActiveLayer^ layer = nullptr;
	float3x2 transform = ds->Transform;
	float dsX = flmax(0.0F, X);
	float dsY = flmax(0.0F, Y);
	float dsWidth = flmin(X + Width, monitor.Width);
	float dsHeight = flmin(Y + Height, monitor.Height);

	if (this->background != nullptr) {
		ds->FillRoundedRectangle(X, Y, Width, Height,
			this->background_corner_radius, this->background_corner_radius,
			this->background);
	}

	for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
		try {
#endif
			decorator->draw_before(ds, X, Y, Width, Height);
#ifdef _DEBUG
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Critical, L"%s: predecorating: %s", this->name()->Data(), e->Message->Data());
		}
#endif
	}

	if (this->head_graphlet != nullptr) {
		IGraphlet* child = this->head_graphlet;
		float gx, gy, gwidth, gheight;
		
		do {
			GraphletInfo* info = GRAPHLET_INFO(child);

			if (unsafe_graphlet_unmasked(info, this->mode)) {
				child->fill_extent(info->x, info->y, &gwidth, &gheight);

				gx = (info->x + this->translate_x) * this->scale_x + X;
				gy = (info->y + this->translate_y) * this->scale_y + Y;
				
				if (rectangle_overlay(gx, gy, gx + gwidth, gy + gheight, dsX, dsY, dsWidth, dsHeight)) {
					if (info->rotation == 0.0F) {
						layer = ds->CreateLayer(info->alpha, Rect(gx, gy, gwidth, gheight));
					} else {
						float cx = gx + gwidth * 0.5F;
						float cy = gy + gheight * 0.5F;

						ds->Transform = make_rotation_matrix(info->rotation, cx, cy, X, Y);
						layer = ds->CreateLayer(info->alpha, Rect(gx, gy, gwidth, gheight));
					}

					for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
						try {
#endif
							decorator->draw_before_graphlet(child, ds, gx, gy, gwidth, gheight, info->selected);
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
							child->draw(ds, gx, gy, gwidth, gheight);
						} else {
							child->draw_progress(ds, gx, gy, gwidth, gheight);
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
							decorator->draw_after_graphlet(child, ds, gx, gy, gwidth, gheight, info->selected);
#ifdef _DEBUG
						} catch (Platform::Exception^ e) {
							this->get_logger()->log_message(Log::Critical, L"%s: postdecorating graphlet: %s",
								this->name()->Data(), e->Message->Data());
						}
#endif
					}

					if (info->selected) {
						this->draw_visible_selection(ds, gx, gy, gwidth, gheight);
					}

					delete layer; // Must Close the Layer Explicitly, it is C++/CX's quirk.
					ds->Transform = transform;
				}
			}

			child = info->next;
		} while (child != this->head_graphlet);
	}

#ifdef _DEBUG
	if (this->figure_track != nullptr) {
		ds->DrawGeometry(this->figure_track, X, Y, Colours::Highlight, this->track_thickness);
	}
#endif

	for (IPlanetDecorator* decorator : this->decorators) {
#ifdef _DEBUG
		try {
#endif
			decorator->draw_after(ds, X, Y, Width, Height);
#ifdef _DEBUG
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Critical, L"%s: postdecorating: %s", this->name()->Data(), e->Message->Data());
		}
#endif
	}

	if (this->keyboard->shown()) {
		float width, height;

		this->keyboard->fill_extent(0.0F, 0.0F, &width, &height);
		this->keyboard->draw(ds, this->keyboard_x + X, this->keyboard_y + Y, width, height);
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

Platform::String^ IPlanet::display_name() {
	return speak(this->caption);
}

IScreen* IPlanet::master() {
	IScreen* screen = nullptr;

	if (this->info != nullptr) {
		screen = this->info->master;
	}

	return screen;
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
		width = this->info->master->actual_width(this);
	}

	return width;
}

float IPlanet::actual_height() {
	float height = 0.0F;

	if (this->info != nullptr) {
		height = this->info->master->actual_height(this);
	}

	return height;
}

float IPlanet::min_width() {
	float width = 0.0F;

	if (this->info != nullptr) {
		width = this->info->master->min_width();
	}

	return width;
}

float IPlanet::min_height() {
	float height = 0.0F;

	if (this->info != nullptr) {
		height = this->info->master->min_height();
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

void IPlanet::begin_update_sequence() {
	if (this->info != nullptr) {
		this->info->master->begin_update_sequence();
	}
}

bool IPlanet::in_update_sequence() {
	return ((this->info != nullptr) && this->info->master->in_update_sequence());
}

void IPlanet::end_update_sequence() {
	if (this->info != nullptr) {
		this->info->master->end_update_sequence();
	}
}

bool IPlanet::needs_update() {
	return ((this->info != nullptr) && this->info->master->needs_update());
}

void IPlanet::notify_graphlet_updated(ISprite* g) {
	if (this->info != nullptr) {
		this->info->master->notify_graphlet_updated(g);
	}
}

void IPlanet::enter_critical_section() {
	if (this->info != nullptr) {
		this->info->master->enter_critical_section();
	}
}

void IPlanet::enter_shared_section() {
	if (this->info != nullptr) {
		this->info->master->enter_shared_section();
	}
}

void IPlanet::leave_critical_section() {
	if (this->info != nullptr) {
		this->info->master->leave_critical_section();
	}
}

void IPlanet::leave_shared_section() {
	if (this->info != nullptr) {
		this->info->master->leave_shared_section();
	}
}

void IPlanet::collapse() {
	this->erase();
}

CanvasRenderTarget^ IPlanet::take_snapshot(float width, float height, CanvasSolidColorBrush^ bgcolor, float dpi) {
	return this->take_snapshot(0.0F, 0.0F, width, height, bgcolor, dpi);
}

CanvasRenderTarget^ IPlanet::take_snapshot(float x, float y, float width, float height, CanvasSolidColorBrush^ bgcolor, float dpi) {
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	CanvasRenderTarget^ snapshot = ref new CanvasRenderTarget(shared_dc, width, height, dpi);
	CanvasDrawingSession^ ds = snapshot->CreateDrawingSession();
	
	if (bgcolor == nullptr) {
		ds->Clear(Colours::Background->Color);
	} else {
		ds->Clear(bgcolor->Color);
	}

	if ((x != 0.0F) || (y != 0.0F)) {
		width += x;
		height += y;
	}

	this->enter_shared_section();
	this->draw(ds, -x, -y, width, height);
	this->leave_shared_section();

	return snapshot;
}

void IPlanet::save(Platform::String^ path, float width, float height, CanvasSolidColorBrush^ bgcolor, float dpi) {
	this->save(path, 0.0F, 0.0F, width, height, bgcolor, dpi);
}

void IPlanet::save(Platform::String^ path, float x, float y, float width, float height, CanvasSolidColorBrush^ bgcolor, float dpi) {
	CanvasRenderTarget^ snapshot = this->take_snapshot(x, y, width, height, bgcolor, dpi);

	if (path_only(path) == nullptr) {
		CreationCollisionOption oie = CreationCollisionOption::OpenIfExists;
		CreationCollisionOption re = CreationCollisionOption::ReplaceExisting;
		Platform::String^ root = Package::Current->DisplayName;
		Platform::String^ subroot = this->name();

		/** WARNING: Stupid Windows 10
		 * Saving through `IRandomAccessStream` is the only working way,
		 * and the `CanvasBitmapFileFormat::Auto` option is lost.
		 */

		create_task(KnownFolders::PicturesLibrary->CreateFolderAsync(root, oie)).then([=](task<StorageFolder^> getting) {
			return create_task(getting.get()->CreateFolderAsync(subroot, oie)).then([=](task<StorageFolder^> subgetting) {
				return create_task(subgetting.get()->CreateFileAsync(path, re)).then([=](task<StorageFile^> creating) {
					return create_task(creating.get()->OpenAsync(FileAccessMode::ReadWrite)).then([=](task<IRandomAccessStream^> opening) {
						return create_task(snapshot->SaveAsync(opening.get(), CanvasBitmapFileFormat::Png, 1.0F));
					});
				});
			});
		}).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice,
					L"planet[%s] has been saved to [My Picture]\\%s\\%s\\%s",
					this->name()->Data(), root->Data(), subroot->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic,
					L"failed to save planet[%s] to [My Pictures]\\%s\\%s\\%s: %s",
					this->name()->Data(), root->Data(), subroot->Data(), path->Data(), e->Message->Data());
			}
		});
	} else {
		create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
			try {
				saving.get();

				this->get_logger()->log_message(Log::Notice,
					L"planet[%s] has been saved to %s",
					this->name()->Data(), path->Data());
			} catch (Platform::Exception^ e) {
				this->get_logger()->log_message(Log::Panic,
					L"failed to save planet[%s] to %s: %s",
					this->name()->Data(), path->Data(), e->Message->Data());
			}
		});
	}
}

void IPlanet::save_logo(float logo_width, float logo_height, Platform::String^ path, float dpi) {
	CanvasRenderTarget^ logo = nullptr;
	float x, y, width, height;

	this->fill_graphlets_boundary(&x, &y, &width, &height);

	if (width <= 0.0F) {
		width = 1240.0F;
		height = 600.0F;
	}

	if (logo_width == 0.0F) {
		logo_width = width;
	} else if (logo_width < 0.0F) {
		logo_width *= -width;
	}

	if (logo_height == 0.0F) {
		logo_height = height;
	} else if (logo_height < 0.0F) {
		logo_height *= -height;
	}

	if ((logo_width == width) && (logo_height == height)) {
		logo = this->take_snapshot(x, y, width, height, Colours::Transparent, dpi);
	} else {
		CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
		CanvasRenderTarget^ src = this->take_snapshot(x, y, width, height, Colours::Transparent, dpi);
		CanvasRenderTarget^ dest = ref new CanvasRenderTarget(shared_dc, logo_width, logo_height, dpi);
		CanvasDrawingSession^ ds = dest->CreateDrawingSession();

		ds->DrawImage(src, Rect(0.0F, 0.0F, logo_width, logo_height));
		logo = dest;
	}


	if (path == nullptr) {
		path = "logo-";
		path += logo_width.ToString();
		path += "x";
		path += logo_height.ToString();
		path += ".png"; // in case the dimension of logo is not an integer;

		path = ms_apptemp_file(path, ".png");
	}

	Concurrency::create_task(logo->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F))
		.then([=](Concurrency::task<void> saving) {
		try {
			saving.get();

			this->get_logger()->log_message(Log::Notice,
				L"Logo[%s] has been saved to %s",
				this->name()->Data(), path->Data());
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Panic,
				L"failed to save logo[%s] to %s: %s",
				this->name()->Data(), path->Data(), e->Message->Data());
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

bool IPlanet::fill_graphlet_location(IGraphlet* g, float* x, float* y, GraphletAnchor a) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	return this->fill_graphlet_location(g, x, y, fx, fy);
}

void IPlanet::insert(IGraphlet* g, float x, float y, GraphletAnchor a, float dx, float dy) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	this->insert(g, x, y, fx, fy, dx, dy);
}

void IPlanet::insert(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, GraphletAnchor a, float dx, float dy) {
	float tfx, tfy, fx, fy;

	graphlet_anchor_fraction(ta, &tfx, &tfy);
	graphlet_anchor_fraction(a, &fx, &fy);

	this->insert(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::insert(IGraphlet* g, IGraphlet* target, float tfx, float tfy, GraphletAnchor a, float dx, float dy) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	this->insert(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::insert(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, float fx, float fy, float dx, float dy) {
	float tfx, tfy;

	graphlet_anchor_fraction(ta, &tfx, &tfy);
	
	this->insert(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::insert(IGraphlet* g, IGraphlet* xtarget, float xfx, IGraphlet* ytarget, float yfy, GraphletAnchor a, float dx, float dy) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	this->insert(g, xtarget, xfx, ytarget, yfy, fx, fy, dx, dy);
}

void IPlanet::move_to(IGraphlet* g, float x, float y, GraphletAnchor a, float dx, float dy) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	this->move_to(g, x, y, fx, fy, dx, dy);
}

void IPlanet::move_to(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, GraphletAnchor a, float dx, float dy) {
	float tfx, tfy, fx, fy;

	graphlet_anchor_fraction(ta, &tfx, &tfy);
	graphlet_anchor_fraction(a, &fx, &fy);

	this->move_to(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::move_to(IGraphlet* g, IGraphlet* target, float tfx, float tfy, GraphletAnchor a, float dx, float dy) {
	float fx, fy;

	graphlet_anchor_fraction(a, &fx, &fy);

	this->move_to(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::move_to(IGraphlet* g, IGraphlet* target, GraphletAnchor ta, float fx, float fy, float dx, float dy) {
	float tfx, tfy;

	graphlet_anchor_fraction(ta, &tfx, &tfy);

	this->move_to(g, target, tfx, tfy, fx, fy, dx, dy);
}

void IPlanet::move_to(IGraphlet* g, IGraphlet* xtarget, float xfx, IGraphlet* ytarget, float yfy, GraphletAnchor a, float dx, float dy) {
	float fx, fy;
	
	graphlet_anchor_fraction(a, &fx, &fy);

	this->move_to(g, xtarget, xfx, ytarget, yfy, fx, fy, dx, dy);
}

/*************************************************************************************************/
IHeadUpPlanet::IHeadUpPlanet(Platform::String^ caption, unsigned int initial_mode) : Planet(caption, initial_mode) {}

void IHeadUpPlanet::fill_margin(float* top, float* right, float* bottom, float* left) {
	SET_BOX(top, 0.0F);
	SET_BOX(right, 0.0F);
	SET_BOX(bottom, 0.0F);
	SET_BOX(left, 0.0F);
}
