#include <map>

#include "page/gauge.hpp"
#include "decorator/background.hpp"
#include "decorator/cell.hpp"
#include "configuration.hpp"

#include "graphlet/symbol/alarmlet.hpp"
#include "graphlet/symbol/gaugelet.hpp"
#include "graphlet/textlet.hpp"

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
		this->font = make_text_format("Microsoft YaHei", design_to_application_height(33.75F));
		this->fgcolor = Colours::GhostWhite;

		this->decorator->reference();
	}

public:
	void load_and_flow(float width, float height) {
		TextExtent ts = get_text_extent("Yacht", this->font);
		float gwidth = design_to_application_width(80.0F);
		float gheight = height * 0.5F * 0.618F;
		float gapsize = ts.height * 0.5F;

		this->load_gauge(GGauge::WasteTank, 0.25F, 0.20F, gwidth, gheight, gapsize);
		this->load_gauge(GGauge::BlackTank, 0.75F, 0.20F, gwidth, gheight, gapsize);
		this->load_gauge(GGauge::FreshTank, 0.25F, 0.70F, gwidth, gheight, gapsize);
		this->load_gauge(GGauge::FuelTank,  0.75F, 0.70F, gwidth, gheight, gapsize);

		{ // load alarms
			float alarm_x, alarm_y, alarm_width;
			float offset = ts.height;

			this->decorator->fill_cell_extent(1, &alarm_x, &alarm_y, &alarm_width, nullptr);
			for (unsigned int idx = 0; idx < static_cast<unsigned int>(GAlarm::_); idx++) {
				GAlarm id = static_cast<GAlarm>(idx);

				this->alarms[id] = new Alarmlet(ts.height);
				this->lblalarms[id] = new Labellet(speak(id.ToString()), this->font, this->fgcolor);

				if (idx == 0) {
					float base_x = alarm_x + alarm_width * 0.618F;
					float base_y = alarm_y + offset;

					this->master->insert(this->alarms[id], base_x + offset, base_y);
					this->master->insert(this->lblalarms[id], base_x - offset, base_y, GraphletAnchor::RT);
				} else {
					GAlarm pid = static_cast<GAlarm>(idx - 1);

					this->master->insert(this->alarms[id], this->alarms[pid], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, offset);
					this->master->insert(this->lblalarms[id], this->lblalarms[pid], GraphletAnchor::RB, GraphletAnchor::RT, 0.0F, offset);
				}
			}
		}
	}

public:
	void on_analog_input_data(uint8* db4, size_t size, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->gauges[GGauge::FuelTank]->set_value(AI_flref(db4,  117U));
		this->gauges[GGauge::FreshTank]->set_value(AI_flref(db4, 119U));
		this->gauges[GGauge::BlackTank]->set_value(AI_flref(db4, 121U));
		this->gauges[GGauge::WasteTank]->set_value(AI_flref(db4, 123U));
		
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	void load_gauge(GGauge id, float fx, float fy, float gwidth, float gheight, float gapsize) {
		float anchor_x, anchor_y;

		this->decorator->fill_cell_anchor(0, fx, fy, &anchor_x, &anchor_y);

		this->gauges[id] = new LevelGaugelet(gwidth, gheight, 3000.0F, 0U, Colours::Yellow);
		this->lblgauges[id] = new Labellet(speak(id.ToString()), this->font, this->fgcolor);
		
		this->master->insert(this->gauges[id], anchor_x, anchor_y, GraphletAnchor::CC);
		this->master->insert(this->lblgauges[id], this->gauges[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
	}

// never deletes these graphlets mannually
private:
	std::map<GGauge, IGaugelet*> gauges;
	std::map<GGauge, Labellet*> lblgauges;
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
		float padding = design_to_application_height(8.0F);
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
