#pragma once

namespace WarGrey::SCADA {
	template<class Base, typename IDEnum>
	private class Credit final : public Base {
		using Base::Base;

	public:
		IDEnum id;
	};
}
