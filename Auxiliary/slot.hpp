#pragma once

#define CAS_SLOT(var, value) if (var == nullptr) var = (value)
#define CAS_SLOTS(var1, var2, value) { auto v = value; CAS_SLOT(var1, v); CAS_SLOT(var2, v); }
#define CAS_VALUES(var1, val1, var2, val2) CAS_SLOT(var1, val1); CAS_SLOT(var2, val2)
