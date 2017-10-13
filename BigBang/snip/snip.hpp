#pragma once

#include "control.hxx"
#include "forward.hpp"

#define SET_BOX(var, value) if (var != nullptr) (*var) = (value)
#define SET_BOXES(var1, var2, value) { auto v = value; SET_BOX(var1, v); SET_BOX(var2, v); }
#define SET_VALUES(var1, val1, var2, val2) SET_BOX(var1, val1); SET_BOX(var2, val2)

namespace WarGrey::SCADA {
    private interface class ISnipInfo : public Microsoft::Graphics::Canvas::ICanvasResourceCreator {
    public:
        property WarGrey::SCADA::Win2DControl^ master { WarGrey::SCADA::Win2DControl^ get(); };
    };

    private class Snip abstract {
    public:
        virtual ~Snip() {};

    public:
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) = 0;
        virtual void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr,
            float* bspace = nullptr, float* tspace = nullptr, float* lspace = nullptr, float* rspace = nullptr) = 0;
        
    public:
        ISnipInfo^ info;

    public:
        Snip* next;
        Snip* prev;
    };
}
