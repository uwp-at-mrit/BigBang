#pragma once

namespace WarGrey::SCADA {
	Platform::String^ speak(Platform::String^ word);
	Platform::String^ dbspeak(Platform::String^ field);

	template<typename E>
	Platform::String^ speak(E id, Platform::String^ prefix) {
		Platform::String^ suffix = id.ToString() + ":";

		if (prefix == nullptr) {
			suffix = ":" + suffix;
		} else {
			suffix = ":" + prefix + "_" + suffix;
		}

		return WarGrey::SCADA::speak(suffix);
	}

	template<typename E>
	Platform::String^ speak(E id) {
		return WarGrey::SCADA::speak(id.ToString());
	}

	template<typename E>
	Platform::String^ dbspeak(E id) {
		return WarGrey::SCADA::dbspeak(":" + id.ToString() + ":");
	}

	private class ITongue abstract {
	public:
		unsigned int ToIndex();
		Platform::String^ ToString(); // return the identity of the instance, analogous to enum.ToString();
		Platform::String^ ToLocalString();

	public:
		virtual unsigned int min_index() { return 0; }
		virtual unsigned int max_index() = 0; // Notice: it is `max`, not `max + 1`;

	protected:
		static bool exists(Platform::String^ name, int index);

	protected:
		ITongue(Platform::String^ name, unsigned int index);

	protected:
		int search_sibling_index(int delta);
		int unsafe_compare(ITongue* instance);

	private:
		Platform::String^ type;
		unsigned int index;
	};

	template<typename E>
	private class Tongue abstract : public WarGrey::SCADA::ITongue {
	public:
		E* foreward() { return Tongue<E>::SafeSiblingTongue(this->search_sibling_index(1)); }
		E* backward() { return Tongue<E>::SafeSiblingTongue(this->search_sibling_index(-1)); }

	public:
		bool eq(E* instance) { return (this->unsafe_compare(instance) == 0); }
		bool lt(E* instance) { return (this->unsafe_compare(instance) <  0); }
		bool le(E* instance) { return (this->unsafe_compare(instance) <= 0); }
		bool gt(E* instance) { return (this->unsafe_compare(instance) >  0); }
		bool ge(E* instance) { return (this->unsafe_compare(instance) >= 0); }

	protected:
		Tongue(unsigned int idx) : ITongue(E::type(), idx) { }

	protected:
		static E* UnsafeTongue(unsigned int idx) {
			static std::map<int, E*> selves;
			auto lt = selves.find(idx);
			E* self = nullptr;

			if (lt != selves.end()) {
				self = lt->second;
			} else {
				self = new E(idx);
				selves.insert(std::pair<int, E*>(idx, self));
			}

			return self;
		}

		static E* SafeTongue(unsigned int index) {
			return (ITongue::exists(E::type(), index) ? Tongue<E>::UnsafeTongue(index) : nullptr);
		}

	private:
		static E* SafeSiblingTongue(int index) {
			return ((index >= 0) ? Tongue<E>::UnsafeTongue(index) : nullptr);
		}
	};
}
