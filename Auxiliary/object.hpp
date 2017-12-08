#pragma once

namespace WarGrey::SCADA {
	private class SharedObject abstract {
	public:
		virtual ~SharedObject() noexcept {};

	public:
		void reference() { this->refcount++; };
		void destroy() { if (this->refcount < 2) { delete this; } else { this->refcount--; }; }

	private:
		int refcount = 0;
    };
}
