#pragma once

#include "device/editor.hpp"

#include "graphlet/filesystem/vessel/trailing_suction_dredgerlet.hpp"

namespace WarGrey::SCADA {
	class TrailingSuctionDredgerSelf;

	private class TrailingSuctionDredgerPlanet : public WarGrey::SCADA::EditorPlanet {
	public:
		virtual ~TrailingSuctionDredgerPlanet() noexcept;
		TrailingSuctionDredgerPlanet(Platform::String^ default_vessel = "vessel");

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
		void on_graphlet_ready(WarGrey::SCADA::IGraphlet* g) override;
		IGraphlet* thumbnail_graphlet() override;

	protected:
		void on_apply() override;
		void on_reset() override;
		bool on_edit(WarGrey::SCADA::Dimensionlet* dim) override;

	private:
		WarGrey::SCADA::TrailingSuctionDredgerSelf* self;
	};
}
