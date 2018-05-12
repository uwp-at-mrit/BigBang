#include <map>

#include "page/propulsion.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/circuit/switchlet.hpp"
#include "graphlet/symbol/circuit/machinelet.hpp"
#include "graphlet/symbol/circuit/accumulatorlet.hpp"
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
	G1, G2, G3, M1, T1, T2, M2, B1,
	ShorePower, PowerStation1, PowerStation2,
	Generator1, Generator2, Propeller1, Propeller2, SolarInverter, Accumulator,
	// switches
	Ssp, Stt, Sgg1, Sgg2, Smp1, Smp2, Stp1, Stp2, Sgs, Sbs,
	SP, _,
	g1, m1, t1, t2, g2, m2, g3, b1
};

static float line_thickness = 3.0F;
static MachineStyle vdc_style = { Colours::GhostWhite, Colours::GhostWhite };

/*************************************************************************************************/
private class LineDiagram final : public PLCConfirmation {
public:
	LineDiagram(PropulsionPage* master, float gridsize) : master(master), gridsize(gridsize) {
		this->label_color = Colours::DimGray;
		this->diagram_color = Colours::GhostWhite;
		this->label_font = make_text_format("Microsoft YaHei", gridsize);
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
		turtle->move_up(3, PD::G3)->move_up(3, PD::Sgs)->move_up(3, PD::SolarInverter)->jump_back();

		turtle->move_right(5, PD::b1);
		turtle->move_down(3, PD::B1)->move_down(3, PD::Sbs)->move_down(2, PD::Accumulator)->move_down();

		this->diagram = new Tracklet<PD>(turtle, line_thickness, Colours::GhostWhite);
		this->accumulator = new Accumulatorlet(gridsize, line_thickness);
		
		this->master->insert(this->diagram);
		this->master->insert(this->accumulator);

		this->load_graphlets(this->machines, MachineShape::Circle, PD::Generator1, PD::Propeller2, this->gridsize, 0.0);
		this->load_graphlets(this->machines, MachineShape::Box, PD::G1, PD::G3, this->gridsize, 0.0);
		this->load_graphlets(this->machines, MachineShape::Box, PD::M1, PD::B1, this->gridsize, 180.0);

		this->load_graphlets(this->switches, PD::Ssp,  PD::Stt, this->gridsize, 0.0);
		this->load_graphlets(this->switches, PD::Sgg1, PD::Sbs, this->gridsize, -90.0);
		this->load_graphlets(this->powers, PD::ShorePower, PD::PowerStation2, this->gridsize, 0.0);

		this->load_graphlets(this->labels, PD::G1, PD::B1);
		this->load_graphlets(this->captions, PD::ShorePower, PD::Accumulator);
	}

	void reflow(float width, float height) {
		this->master->move_to(this->diagram, width * 0.5F, height * 0.5F, GraphletAlignment::CC);
		this->diagram->map_graphlet_at_anchor(this->accumulator, PD::Accumulator, GraphletAlignment::CT);
		
		for (auto lt = this->machines.begin(); lt != this->machines.end(); lt++) {
			GraphletAlignment align = GraphletAlignment::CC;

			switch (lt->first) {
			case PD::Generator1: case PD::Generator2: align = GraphletAlignment::CB; break;
			case PD::Propeller1: case PD::Propeller2: align = GraphletAlignment::CT; break;
			}

			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, align);
		}

		for (auto lt = this->switches.begin(); lt != this->switches.end(); lt++) {
			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, GraphletAlignment::CC);
		}

		for (auto lt = this->powers.begin(); lt != this->powers.end(); lt++) {
			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, GraphletAlignment::CT);
		}

		for (auto lt = this->labels.begin(); lt != this->labels.end(); lt++) {
			this->master->move_to(lt->second, this->machines[lt->first], GraphletAlignment::LC,
				GraphletAlignment::RC, -this->gridsize * 0.5F);
		}

		for (auto lt = this->captions.begin(); lt != this->captions.end(); lt++) {
			switch (lt->first) {
			case PD::Generator1: case PD::Generator2: {
				this->master->move_to(lt->second, this->machines[lt->first], GraphletAlignment::CT, GraphletAlignment::CB);
			}; break;
			case PD::Propeller1: case PD::Propeller2: {
				this->master->move_to(lt->second, this->machines[lt->first], GraphletAlignment::CB, GraphletAlignment::CT);
			}; break;
			case PD::PowerStation1: case PD::PowerStation2: {
				this->master->move_to(lt->second, this->powers[lt->first], GraphletAlignment::CB, GraphletAlignment::CT);
			}; break;
			case PD::ShorePower: {
				this->diagram->map_graphlet_at_anchor(lt->second, PD::SP, GraphletAlignment::CB);
			}; break;
			case PD::Accumulator: {
				this->master->move_to(lt->second, this->accumulator, GraphletAlignment::CB, GraphletAlignment::CT);
			}; break;
			default: {
				this->diagram->map_graphlet_at_anchor(lt->second, lt->first, GraphletAlignment::CC);
			}
			}
		}
	}

private:
	template<class G>
	void load_graphlets(std::map<PD, G*>& gs, PD id0, PD idn, float radius, double degrees) {
		for (PD id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, line_thickness, degrees));
		}
	}

	void load_graphlets(std::map<PD, Machinelet*>& ms, MachineShape shape, PD id0, PD idn, float radius, double degrees) {
		for (PD m = id0; m <= idn; m++) {
			Platform::String^ sign = L"~"; // default for G1, G2, G3, M1, M2, T1, T2

			switch (m) {
			case PD::B1: sign = L"="; break;
			case PD::Generator1: case PD::Generator2: sign = L"G"; break;
			case PD::Propeller1: case PD::Propeller2: sign = L"M"; break;
			}

			ms[m] = this->master->insert_one(new Machinelet(shape, sign, radius, line_thickness, degrees));

			if (shape == MachineShape::Box) {
				ms[m]->set_style(MachineStatus::Normal, vdc_style);
			}
		}
	}

	void load_graphlets(std::map<PD, Labellet*>& ls, PD id0, PD idn) {
		for (PD l = id0; l <= idn; l++) {
			ls[l] = this->master->insert_one(new Labellet(speak(l.ToString()), this->label_font, this->label_color));
		}
	}

	void load_dimensions(std::map<PD, Dimensionlet*>& sts, PD id0, PD idn, Platform::String^ unit, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr) {
		for (PD id = id0; id <= idn; id++) {
			sts[id] = this->master->insert_one(new Dimensionlet(unit, label, subscript));
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<PD>* diagram;
	std::map<PD, Labellet*> labels;
	std::map<PD, Labellet*> captions;
	std::map<PD, Switchlet*> switches;
	std::map<PD, Machinelet*> machines;
	std::map<PD, PowerStationlet*> powers;
	Accumulatorlet* accumulator;

private:
	CanvasTextFormat^ label_font;
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
