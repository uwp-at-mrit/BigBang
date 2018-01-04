#pragma once

#define SET_BOX(var, value) if (var != nullptr) (*var) = (value)
#define SET_BOXES(var1, var2, value) { auto v = value; SET_BOX(var1, v); SET_BOX(var2, v); }
#define SET_VALUES(var1, val1, var2, val2) SET_BOX(var1, val1); SET_BOX(var2, val2)
