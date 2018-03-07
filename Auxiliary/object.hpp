#pragma once

namespace WarGrey::SCADA {
	private class SharedObject abstract {
	public:
		// TODO: these operations should be locked for thread-safety
		void reference() {
			this->refcount++;
		};

		void destroy() {
			if (this->refcount <= 1) {
				delete this;
			} else {
				this->refcount--;
			};
		}

	protected:
		virtual ~SharedObject() noexcept {};

	private:
		int refcount = 0;
    };
}
