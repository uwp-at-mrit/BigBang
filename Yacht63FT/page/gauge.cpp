#include <map>

#include "page/gauge.hpp"
#include "decorator/cell.hpp"
#include "decorator/background.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/device/alarmlet.hpp"
#include "graphlet/dashboard/cylinderlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class GGauge { WasteTank, BlackTank, FreshTank, FuelTank };
private enum class GAlarm { WaterPump, FreshPump, BowLevel, CabinLevel, HoldLevel, SternLevel, StorageLevel, _ };

static unsigned int cell_bgcolor = 0x2E2E2EU;

/*************************************************************************************************/
private class GaugeBoard final : public PLCConfirmation {
public:
	~GaugeBoard() noexcept {
		if (this->decorator != nullptr) {
			this->decorator->destroy();
		}
	}

	GaugeBoard(GaugePage* master, CellDecorator* decorator) : master(master), decorator(decorator) {
		this->font = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(33.75F));
		this->fgcolor = Colours::GhostWhite;

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		TextExtent ts = get_text_extent("Yacht", this->font);
		float gwidth = this->master->sketch_to_application_width(80.0F);
		float gheight = height * 0.5F * 0.618F;
		float gapsize = ts.height * 0.5F;

		this->load_gauge<Cylinderlet>(GGauge::WasteTank,        0.25F, 0.20F, gwidth, gheight, gapsize);
		this->load_gauge<ConcaveCylinderlet>(GGauge::BlackTank, 0.75F, 0.20F, gwidth, gheight, gapsize);
		this->load_gauge<ConcaveCylinderlet>(GGauge::FreshTank, 0.25F, 0.70F, gwidth, gheight, gapsize);
		this->load_gauge<ConvexCylinderlet>(GGauge::FuelTank,   0.75F, 0.70F, gwidth, gheight, gapsize);

		{ // load alarms
			float alarm_x, alarm_y, alarm_width, label_x;
			float vgapsize = ts.height;
			float hgapsize = ts.height * 2.0F;

			this->decorator->fill_cell_extent(1, &alarm_x, &alarm_y, &alarm_width, nullptr);
			label_x = alarm_x + alarm_width * 0.618F - hgapsize * 0.5F;
			for (unsigned int idx = 0; idx < _N(GAlarm); idx++) {
				float label_y = alarm_y + (ts.height + vgapsize) * float(idx) + vgapsize;
				GAlarm id = _E(GAlarm, idx);

				this->lblalarms[id] = new Labellet(speak(id), this->font, this->fgcolor);
				this->alarms[id] = new Alarmlet(ts.height);

				this->master->insert(this->lblalarms[id], label_x, label_y, GraphletAnchor::RT);
				this->master->insert(this->alarms[id],
					this->lblalarms[id], GraphletAnchor::RC,
					GraphletAnchor::LC, hgapsize);
			}
		}
	}

public:
	void on_analog_input_data(uint8* db4, size_t size, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->mcylinders[GGauge::FuelTank]->set_value(AI_ref(db4,  117U));
		this->mcylinders[GGauge::FreshTank]->set_value(AI_ref(db4, 119U));
		this->mcylinders[GGauge::BlackTank]->set_value(AI_ref(db4, 121U));
		this->mcylinders[GGauge::WasteTank]->set_value(AI_ref(db4, 123U));
		
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	template<class Clet>
	void load_gauge(GGauge id, float fx, float fy, float gwidth, float gheight, float gapsize) {
		float anchor_x, anchor_y;

		this->decorator->fill_cell_anchor(0, fx, fy, &anchor_x, &anchor_y);

		this->mcylinders[id] = new Clet(0.0F, 3000.0F, gwidth, gheight);
		this->lblcylinders[id] = new Labellet(speak(id), this->font, this->fgcolor);

		this->master->insert(this->mcylinders[id], anchor_x, anchor_y, GraphletAnchor::CC);
		this->master->insert(this->lblcylinders[id], this->mcylinders[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
	}

// never deletes these graphlets mannually
private:
	std::map<GGauge, ICylinderlet*> mcylinders;
	std::map<GGauge, Labellet*> lblcylinders;
	std::map<GAlarm, Alarmlet*> alarms;
	std::map<GAlarm, Labellet*> lblalarms;
		
private:
	CanvasTextFormat^ font;
	ICanvasBrush^ fgcolor;
	CellDecorator* decorator;
	GaugePage* master;
};

/*************************************************************************************************/
GaugePage::GaugePage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

GaugePage::~GaugePage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void GaugePage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		float padding = this->sketch_to_application_height(8.0F);
		float region_width = (width - padding * 3.0F);
		float region_y = padding;
		float region_height = (height - padding * 2.0F);
		float rliquid_x = padding;
		float rliquid_width = region_width * 0.618F;
		float ralarm_x = padding + rliquid_width + padding;
		float ralarm_width = region_width - rliquid_width;

		Rect cells[2] = {
			Rect(padding, padding, rliquid_width, region_height),
			Rect(ralarm_x, padding, ralarm_width, region_height)
		};

		CellDecorator* regions = new CellDecorator(cell_bgcolor, cells); // don't mind, it's Visual Studio's fault
		GaugeBoard* lb = new GaugeBoard(this, regions);

		lb->load_and_flow(width, height);

		this->dashboard = lb;
		this->set_decorator(regions);
		this->device->append_confirmation_receiver(lb);
	}
}

void GaugePage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
