#pragma once

#define CAS_SLOT(var, value) if (var == nullptr) var = (value)
#define CAS_SLOTS(var1, var2, value) { auto v = value; CAS_SLOT(var1, v); CAS_SLOT(var2, v); }
#define CAS_VALUES(var1, val1, var2, val2) CAS_SLOT(var1, val1); CAS_SLOT(var2, val2)

#define FLCAS_SLOT(var, value) if (var < 0.0) var = (value)
#define FLCAS_SLOTS(var1, var2, value) { auto v = value; FLCAS_SLOT(var1, v); FLCAS_SLOT(var2, v); }
#define FLCAS_VALUES(var1, val1, var2, val2) FLCAS_SLOT(var1, val1); FLCAS_SLOT(var2, val2)

#define ICAS_SLOT(var, value) if (var < 0) var = (value)
#define ICAS_SLOTS(var1, var2, value) { auto v = value; ICAS_SLOT(var1, v); ICAS_SLOT(var2, v); }
#define ICAS_VALUES(var1, val1, var2, val2) ICAS_SLOT(var1, val1); ICAS_SLOT(var2, val2)
