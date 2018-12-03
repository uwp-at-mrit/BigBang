#pragma once

namespace WarGrey::SCADA {
	Platform::String^ speak(Platform::String^ word);
	Platform::String^ speak(Platform::String^ word, Platform::String^ scope);
	Platform::String^ speak(Platform::String^ word, Platform::String^ scope, bool* exists);
	Platform::String^ dbspeak(Platform::String^ field);
	Platform::String^ unitspeak(Platform::String^ unit);

	template<typename E>
	Platform::String^ speak(E id) {
		return WarGrey::SCADA::speak(id.ToString());
	}

	template<typename E>
	Platform::String^ speak(E id, Platform::String^ scope) {
		return WarGrey::SCADA::speak(id.ToString(), scope);
	}

	template<typename E>
	Platform::String^ dbspeak(E id) {
		return WarGrey::SCADA::dbspeak(id.ToString());
	}

	private class ITongue abstract {
	public:
		unsigned int ToIndex();
		Platform::String^ ToString(); // return the identity of the instance, analogous to enum.ToString();
		Platform::String^ ToLocalString();

	protected:
		static bool exists(Platform::String^ name, int index);
		static int sibling_index(Platform::String^ name, unsigned int current, int delta, unsigned int boundary);

	protected:
		ITongue(Platform::String^ name, unsigned int index);

	protected:
		int unsafe_compare(ITongue* instance);

	private:
		Platform::String^ type;
		unsigned int index;
	};

	template<typename E>
	private class Tongue abstract : public WarGrey::SCADA::ITongue {
	public:
		static E* first() { return Tongue<E>::SafeSiblingTongue(E::min_index() - 1, 1, E::max_index()); }
		static E* fromIndex(unsigned int idx) { return Tongue<E>::SafeTongue(idx); }
		static E* last() { return Tongue<E>::SafeSiblingTongue(E::max_index() + 1, -1, E::min_index()); }

	public:
		E* foreward() { return Tongue<E>::SafeSiblingTongue(this->ToIndex(), 1, E::max_index()); }
		E* backward() { return Tongue<E>::SafeSiblingTongue(this->ToIndex(), -1, E::min_index()); }

	public:
		bool eq(E* instance) { return (this->unsafe_compare(instance) == 0); }
		bool lt(E* instance) { return (this->unsafe_compare(instance) <  0); }
		bool le(E* instance) { return (this->unsafe_compare(instance) <= 0); }
		bool gt(E* instance) { return (this->unsafe_compare(instance) >  0); }
		bool ge(E* instance) { return (this->unsafe_compare(instance) >= 0); }

	protected:
		Tongue(unsigned int idx) : ITongue(E::type(), idx) {}

	protected:
		static E* UnsafeTongue(unsigned int idx) {
			static std::map<int, E*> selves;
			auto it = selves.find(idx);
			E* self = nullptr;

			if (it != selves.end()) {
				self = it->second;
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
		static E* SafeSiblingTongue(unsigned int index, int delta, int boundary) {
			int sibling_index = ITongue::sibling_index(E::type(), index, delta, boundary);

			return ((sibling_index >= 0) ? Tongue<E>::UnsafeTongue(sibling_index) : nullptr);
		}
	};
}
