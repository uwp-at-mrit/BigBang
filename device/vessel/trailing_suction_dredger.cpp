#include <ppltasks.h>

#include "device/vessel/trailing_suction_dredger.hpp"

#include "datum/flonum.hpp"
#include "datum/time.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "planet.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::System;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
