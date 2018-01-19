#pragma once

namespace WarGrey::SCADA {
	/**
	 * WARNING
	 *   To use this mechanism correctly, every instance needs to be referenced at least once,
	 *   so that the form "new SubSharedObject()" can be passed as an argument directly when
	 *   invoking methods.
	 *
	 *   Never delete its instances directly. 
	 */
	private class SharedObject abstract {
	public:
		virtual ~SharedObject() noexcept {};

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

	private:
		int refcount = 0;
    };
}
