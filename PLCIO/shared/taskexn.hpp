#pragma once

#include <exception>

namespace WarGrey::SCADA {
	private class task_terminated : public std::exception {
	public:
		task_terminated() noexcept : exception() {}
	};

	private class task_discarded : public std::exception {
	public:
		task_discarded() noexcept : exception() {}
	};
}
