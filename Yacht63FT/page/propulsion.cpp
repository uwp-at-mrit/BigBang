#include <map>

#include "page/propulsion.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/svg/storagecelletv.hpp"
#include "graphlet/svg/solarinverterletv.hpp"
#include "graphlet/symbol/circuit/switchlet.hpp"
#include "graphlet/symbol/circuit/machinelet.hpp"
#include "graphlet/symbol/circuit/converterlet.hpp"
#include "graphlet/symbol/circuit/powerstationlet.hpp"

#include "tongue.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#ifdef _DEBUG
#include "decorator/grid.hpp"
#endif

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum class PD { // order matters
	// labelled stuffs
	B1, M1, M2, T1, T2, G1, G2, G3,
	Generator1, Generator2, Propeller1, Propeller2,
	ShorePower, PowerStation1, PowerStation2,
	SolarInverter, StorageCell,
	// switches
	Ssp, Stt, Sgg1, Sgg2, Smp1, Smp2, Stp1, Stp2, Sgs, Sbs,
	SP, _,
	g1, m1, t1, t2, g2, m2, g3, b1
};

private enum class PS { Normal, Breakdown };

static float line_thickness = 3.0F;

/*************************************************************************************************/
private class LineDiagram final : public PLCConfirmation {
public:
	LineDiagram(PropulsionPage* master, float gridsize) : master(master), gridsize(gridsize) {
		this->label_color = Colours::DarkGray;
		this->diagram_color = Colours::GhostWhite;
		this->label_font = make_text_format("Microsoft YaHei", gridsize);
		this->dim_font = make_text_format("Microsoft YaHei", gridsize * 0.5F);
	}

public:
	void load() {
		Turtle<PD>* turtle = new Turtle<PD>(this->gridsize, true, PD::Generator1);

		turtle->move_down(2, PD::Sgg1)->move_down(2, PD::g1);
		turtle->move_right(5, PD::Ssp)->move_right(2)->move_down(2)->move_right(3)->move_up(3.5F, PD::ShorePower);
		turtle->move_up(2.5F, PD::SP)->move_left(3)->jump_right(3)->move_right(3)->jump_back();

		turtle->move_down(2, PD::G1)->move_down(3, PD::m1);
		turtle->move_down(3, PD::M1)->move_down(3, PD::Smp1)->move_down(2, PD::Propeller1)->jump_back();
		
		turtle->move_right(10, PD::t1);
		turtle->move_down(3, PD::T1)->move_down(3, PD::Stp1)->move_down(2, PD::PowerStation1)->jump_back();

		turtle->move_right(5, PD::Stt)->move_right(5, PD::t2);
		turtle->move_down(3, PD::T2)->move_down(3, PD::Stp2)->move_down(2, PD::PowerStation2)->jump_back();

		turtle->move_right(5, PD::g2);
		turtle->move_up(3, PD::G2)->move_up(3, PD::Sgg2)->move_up(3, PD::Generator2)->jump_back();

		turtle->move_right(5, PD::m2);
		turtle->move_down(3, PD::M2)->move_down(3, PD::Smp2)->move_down(2, PD::Propeller2)->jump_back();

		turtle->move_right(5, PD::g3);
		turtle->move_up(3, PD::G3)->move_up(3, PD::Sgs)->move_up(3, PD::SolarInverter)->move_up()->jump_back();

		turtle->move_right(5, PD::b1);
		turtle->move_down(3, PD::B1)->move_down(3, PD::Sbs)->move_down(2, PD::StorageCell)->move_down();

		this->diagram = this->master->insert_one(new Tracklet<PD>(turtle, line_thickness, Colours::GhostWhite));
		this->storagecell = this->master->insert_one(new StorageCelletv(0.0F, this->gridsize * 2.0F));
		this->inverter = this->master->insert_one(new SolarInverterletv(0.0F, this->gridsize * 2.0F));
		
		this->load_graphlets(this->machines, PD::Generator1, PD::Propeller2, this->gridsize, line_thickness, 0.0);
		this->load_graphlets(this->vfds, PD::G1, PD::G3, this->gridsize, line_thickness, 0.0);
		this->load_graphlets(this->vfds, PD::B1, PD::T2, this->gridsize, line_thickness, 180.0);

		this->load_graphlets(this->switches, PD::Ssp,  PD::Stt, this->gridsize, 0.0);
		this->load_graphlets(this->switches, PD::Sgg1, PD::Sbs, this->gridsize, -90.0);
		this->load_graphlets(this->pstations, PD::ShorePower, PD::PowerStation2, this->gridsize, 0.0);

		this->load_graphlets(this->labels, PD::B1, PD::G3);
		this->load_graphlets(this->captions, PD::Generator1, PD::StorageCell);

		this->load_graphlets(this->rspeeds, PD::Generator1, PD::Generator2, PD::M1, PD::M2, ":rspeed:", "<rspeed>");
		this->load_graphlets(this->powers, PD::B1, PD::Generator2, ":power:", "<power>");
		this->load_graphlets(this->currents, PD::B1, PD::Generator2, ":current:", "<current>");
		this->load_graphlets(this->voltages, PD::B1, PD::G3, ":voltage:", "<voltage>");
		this->load_graphlets(this->temperatures, PD::B1, PD::G3, ":temperature:", "<temperature>");
		this->load_graphlets(this->frequencies, PD::T1, PD::G3, ":frequency:", "<frequency>");
	}

	void reflow(float width, float height) {
		float vfds_xoff = this->gridsize * 0.5F;
		float scap_yoff = this->gridsize * 2.0F;
		float rtms_xoff = this->gridsize * 0.5F;
		float ltms_xoff = this->gridsize * 4.0F;

		this->master->move_to(this->diagram, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		
		this->map_graphlets(this->vfds, GraphletAnchor::CC);
		this->map_graphlets(this->switches, GraphletAnchor::CC);
		this->map_graphlets(this->pstations, GraphletAnchor::CT);
		this->map_graphlets(this->machines, PD::Generator1, PD::Generator2, GraphletAnchor::CB);
		this->map_graphlets(this->machines, PD::Propeller1, PD::Propeller2, GraphletAnchor::CT);

		this->map_graphlets(this->captions, this->machines, PD::Generator1, PD::Generator2, GraphletAnchor::CT, GraphletAnchor::CB);
		this->map_graphlets(this->captions, this->machines, PD::Propeller1, PD::Propeller2, GraphletAnchor::CB, GraphletAnchor::CT);
		this->map_graphlets(this->captions, this->pstations, PD::PowerStation1, PD::PowerStation2, GraphletAnchor::CB, GraphletAnchor::CT);
		
		this->diagram->map_graphlet_at_anchor(this->captions[PD::ShorePower], PD::SP, GraphletAnchor::CB);
		this->diagram->map_graphlet_at_anchor(this->inverter, PD::SolarInverter, GraphletAnchor::CB);
		this->diagram->map_graphlet_at_anchor(this->storagecell, PD::StorageCell, GraphletAnchor::CT);
		this->diagram->map_graphlet_at_anchor(this->captions[PD::SolarInverter], PD::SolarInverter, GraphletAnchor::CB, 0.0F, -scap_yoff);
		this->diagram->map_graphlet_at_anchor(this->captions[PD::StorageCell], PD::StorageCell, GraphletAnchor::CT, 0.0F, scap_yoff);
		
		for (auto lt = this->labels.begin(); lt != this->labels.end(); lt++) {
			switch (lt->first) {
			case PD::G1: {
				this->master->move_to(lt->second, this->vfds[lt->first], GraphletAnchor::RC, GraphletAnchor::LC, vfds_xoff);
			}; break;
			default: {
				this->master->move_to(lt->second, this->vfds[lt->first], GraphletAnchor::LC, GraphletAnchor::RC, -vfds_xoff);
			}
			}
		}

		this->map_metrics(this->machines, PD::Generator1, PD::Generator2, GraphletAnchor::LT, GraphletAnchor::LT, -ltms_xoff);
		this->map_metrics(this->vfds, PD::M1, PD::T2, GraphletAnchor::RT, GraphletAnchor::LB, rtms_xoff);
		this->map_metrics(this->vfds, PD::G1, GraphletAnchor::LT, GraphletAnchor::LB, -ltms_xoff);
		this->map_metrics(this->vfds, PD::G2, PD::G3, GraphletAnchor::RT, GraphletAnchor::LB, rtms_xoff);
		this->map_metrics(this->vfds, PD::B1, GraphletAnchor::RT, GraphletAnchor::LC, rtms_xoff);
	}

public:
	void on_analog_input_data(uint8* db4, size_t size, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->set_values(this->powers,   PD::Generator1, PD::Generator2, db4, 3, 11);
		this->set_values(this->rspeeds,  PD::Generator1, PD::Generator2, db4, 4, 11);
		this->set_values(this->currents, PD::Generator1, PD::Generator2, db4, 5, 11);

		this->set_values(this->powers,       PD::M1, PD::M2, db4, 24, 11);
		this->set_values(this->rspeeds,      PD::M1, PD::M2, db4, 25, 11);
		this->set_values(this->currents,     PD::M1, PD::M2, db4, 26, 11);
		this->set_values(this->voltages,     PD::M1, PD::M2, db4, 27, 11);
		this->set_values(this->temperatures, PD::M1, PD::M2, db4, 28, 11);

		this->set_values(this->powers,       PD::T1, PD::T2, db4, 46, 6);
		this->set_values(this->frequencies,  PD::T1, PD::T2, db4, 48, 6);
		this->set_values(this->currents,     PD::T1, PD::T2, db4, 49, 6);
		this->set_values(this->voltages,     PD::T1, PD::T2, db4, 50, 6);
		this->set_values(this->temperatures, PD::T1, PD::T2, db4, 51, 6);

		this->set_values(this->powers,       PD::B1, db4, 58);
		this->set_values(this->currents,     PD::B1, db4, 59);
		this->set_values(this->voltages,     PD::B1, db4, 60);
		this->set_values(this->temperatures, PD::B1, db4, 61);

		this->set_values(this->voltages,     PD::G1, PD::G2, db4, 6, 11);
		this->set_values(this->temperatures, PD::G1, PD::G2, db4, 7, 11);

		this->set_values(this->powers,       PD::G3, db4, 68);
		this->set_values(this->frequencies,  PD::G3, db4, 69);
		this->set_values(this->currents,     PD::G3, db4, 70);
		this->set_values(this->voltages,     PD::G3, db4, 71);
		this->set_values(this->temperatures, PD::G3, db4, 72);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

private:
	template<class G>
	void load_graphlets(std::map<PD, G*>& gs, PD id0, PD idn, float radius, double degrees) {
		for (PD id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, line_thickness, degrees));
		}
	}

	template<class G>
	void load_graphlets(std::map<PD, G*>& ms, PD id0, PD idn, float radius, float thickness, double degrees) {
		for (PD m = id0; m <= idn; m++) {
			Platform::String^ sign = L"~"; // default for G1, G2, G3, M1, M2, T1, T2

			switch (m) {
			case PD::B1: sign = L"="; break;
			case PD::Generator1: case PD::Generator2: sign = L"G"; break;
			case PD::Propeller1: case PD::Propeller2: sign = L"M"; break;
			}

			ms[m] = this->master->insert_one(new G(sign, radius, thickness, degrees));
		}
	}

	void load_graphlets(std::map<PD, Labellet*>& ls, PD id0, PD idn) {
		for (PD l = id0; l <= idn; l++) {
			ls[l] = this->master->insert_one(new Labellet(speak(l.ToString()), this->label_font, this->label_color));
		}
	}

	void load_graphlets(std::map<PD, Dimensionlet*>& ds, PD id0, PD idn, Platform::String^ label, Platform::String^ unit) {
		for (PD id = id0; id <= idn; id++) {
			ds[id] = new Dimensionlet(unit, label, this->dim_font, Colours::Yellow, Colours::GhostWhite);
			
			this->master->insert(ds[id]);
		}
	}

	void load_graphlets(std::map<PD, Dimensionlet*>& ds, PD id10, PD id1n, PD id20, PD id2n, Platform::String^ label, Platform::String^ unit) {
		this->load_graphlets(ds, id10, id1n, label, unit);
		this->load_graphlets(ds, id20, id2n, label, unit);
	}

private:
	template<class G>
	void map_graphlets(std::map<PD, G*>& gs, GraphletAnchor a) {
		for (auto lt = gs.begin(); lt != gs.end(); lt++) {
			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, a);
		}
	}

	template<class G>
	void map_graphlets(std::map<PD, G*>& gs, PD id0, PD idn, GraphletAnchor a) {
		for (PD id = id0; id <= idn; id++) {
			this->diagram->map_graphlet_at_anchor(gs[id], id, a);
		}
	}

	template<class G, class T>
	void map_graphlets(std::map<PD, G*>& gs, std::map<PD, T*>& ts, PD id0, PD idn, GraphletAnchor ta, GraphletAnchor a) {
		for (PD id = id0; id <= idn; id++) {
			this->master->move_to(gs[id], ts[id], ta, a);
		}
	}

	template<class G>
	void map_metrics(std::map<PD, G*>& ms, PD id, GraphletAnchor ta, GraphletAnchor a, float xoff) {
		this->map_metrics(ms, id, id, ta, a, xoff);
	}

	template<class G>
	void map_metrics(std::map<PD, G*>& ms, PD id0, PD idn, GraphletAnchor ta, GraphletAnchor a, float xoff) {
		for (PD id = id0; id <= idn; id++) {
			if (this->voltages.find(id) == this->voltages.end()) { // for Generators
				this->master->move_to(this->powers[id], ms[id], ta, a, xoff);
				this->master->move_to(this->rspeeds[id], this->powers[id], GraphletAnchor::LB, GraphletAnchor::LT);
				this->master->move_to(this->currents[id], this->rspeeds[id], GraphletAnchor::LB, GraphletAnchor::LT);
			} else { // for Converters
				this->master->move_to(this->voltages[id], ms[id], ta, a, xoff);
				this->master->move_to(this->currents[id], this->voltages[id], GraphletAnchor::LB, GraphletAnchor::LT);
				this->master->move_to(this->powers[id], this->currents[id], GraphletAnchor::LB, GraphletAnchor::LT);

				if (this->frequencies.find(id) != this->frequencies.end()) {
					this->master->move_to(this->frequencies[id], this->powers[id], GraphletAnchor::LB, GraphletAnchor::LT);
					this->master->move_to(this->temperatures[id], this->frequencies[id], GraphletAnchor::LB, GraphletAnchor::LT);
				} else if (this->rspeeds.find(id) != this->rspeeds.end()) {
					this->master->move_to(this->rspeeds[id], this->powers[id], GraphletAnchor::LB, GraphletAnchor::LT);
					this->master->move_to(this->temperatures[id], this->rspeeds[id], GraphletAnchor::LB, GraphletAnchor::LT);
				} else {
					this->master->move_to(this->temperatures[id], this->powers[id], GraphletAnchor::LB, GraphletAnchor::LT);
				}
			}
		}
	}

private:
	void set_values(std::map<PD, Dimensionlet*>& ms, PD id, uint8* db, size_t idx) {
		this->set_values(ms, id, id, db, idx, 0);
	}
	
	void set_values(std::map<PD, Dimensionlet*>& ms, PD id0, PD idn, uint8* db, size_t idx, size_t acc) {
		for (PD id = id0; id <= idn; id++) {
			ms[id]->set_value(AI_ref(db, idx));
			idx += acc;
		}
	}
	
// never deletes these graphlets mannually
private:
	Tracklet<PD>* diagram;
	std::map<PD, Dimensionlet*> powers;
	std::map<PD, Dimensionlet*> rspeeds;
	std::map<PD, Dimensionlet*> currents;
	std::map<PD, Dimensionlet*> voltages;
	std::map<PD, Dimensionlet*> temperatures;
	std::map<PD, Dimensionlet*> frequencies;
	std::map<PD, Labellet*> labels;
	std::map<PD, Labellet*> captions;
	std::map<PD, Switchlet*> switches;
	std::map<PD, Machinelet*> machines;
	std::map<PD, Converterlet*> vfds;
	std::map<PD, PowerStationlet*> pstations;
	StorageCelletv* storagecell;
	SolarInverterletv* inverter;

private:
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ dim_font;
	ICanvasBrush^ label_color;
	ICanvasBrush^ diagram_color;
	PropulsionPage* master;
	float gridsize;
};

/*************************************************************************************************/
PropulsionPage::PropulsionPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

PropulsionPage::~PropulsionPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void PropulsionPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		float gridsize = 24.0F;
		LineDiagram* diagram = new LineDiagram(this, gridsize);
		
		diagram->load();

#ifdef _DEBUG
		this->set_decorator(new GridDecorator(gridsize, gridsize));
#endif

		this->dashboard = diagram;
		this->device->append_confirmation_receiver(diagram);
	}
}

void PropulsionPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<LineDiagram*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->reflow(width, height);
	}
}

void PropulsionPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
