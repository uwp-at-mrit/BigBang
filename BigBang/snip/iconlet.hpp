#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
	private class Iconlet : public WarGrey::SCADA::ISnip {
	public:
		Iconlet(float size) : size(size) {}

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	protected:
		float size;
	};
}
