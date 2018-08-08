#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
#define __MODULE__ WarGrey::SCADA::module_name(__FILE__)
#define _speak(word) WarGrey::SCADA::speak(word, module_name(__FILE__))

	Platform::String^ module_name(Platform::String^ src);
}
