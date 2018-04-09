#include "static_test.hpp"

#include "pch.h"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

float WarGrey::SCADA::fltimespan(float a) {
	TimeSpan ts = make_timespan_from_rate(24);

	return float(ts.Duration) + a;
}
