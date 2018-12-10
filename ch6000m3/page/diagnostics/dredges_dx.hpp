#pragma once

#include "satellite.hpp"
#include "plc.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/textlet.hpp"

namespace WarGrey::SCADA {
	private enum class DX { PS, SB };
	private enum class DxPosition { Trunnion, Intermediate, Draghead };

	private class DredgesDiagnostics : public WarGrey::SCADA::ICreditSatellite<WarGrey::SCADA::DxPosition> {
	public:
		~DredgesDiagnostics() noexcept;
		DredgesDiagnostics(WarGrey::SCADA::DX side, WarGrey::SCADA::PLCMaster* plc);

	public:
		void fill_satellite_extent(float* width, float* height) override;

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	protected:
		void on_id_changed(DxPosition id) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;

	private:
		float title_height;
		float vgapsize;

	private: // never delete these graphlets manually
		WarGrey::SCADA::Rectanglet* titlebar;
		WarGrey::SCADA::Labellet* title;
	};
}
