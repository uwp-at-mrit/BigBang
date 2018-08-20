#pragma once
#pragma warning(disable: 4250)

namespace WarGrey::SCADA {
	template<class Base, typename IDEnum>
	private class Credit final : public Base {
		using Base::Base;

	public:
		IDEnum id;
	};
}
