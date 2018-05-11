#include <map>

#include "page/propulsion.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/switchlet.hpp"

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
	// captions
	Generator1, SP, G1, M1, P1, T1, T2, G2, Generator2, M2, Propeller2, G3, SolarInverter, B1, StorageBattery,
	// power providers
	ShorePower, PowerStation1, PowerStation2,
	// switches
	Ssp, Stt, Sgg1, Sgg2, Smp1, Smp2, Stp1, Stp2, Sgs, Sbs,
	_,
	g1, m1, t1, t2, g2, m2, g3, b1
};

static float line_thickness = 3.0F;

/*************************************************************************************************/
private class LineDiagram final : public PLCConfirmation {
public:
	LineDiagram(PropulsionPage* master) : master(master) {}

public:
	void load(float gridsize) {
		Turtle<PD>* turtle = new Turtle<PD>(gridsize, true, PD::Generator1);

		turtle->move_down(2, PD::Sgg1)->move_down(2, PD::g1);
		turtle->move_right(5, PD::Ssp)->move_right(2)->move_down(2)->move_right(3)->move_up(4, PD::SP);
		turtle->move_up(2, PD::SP)->move_left(3)->jump_right(3)->move_right(3)->jump_back();

		turtle->move_down(2, PD::G1)->move_down(3, PD::m1);
		turtle->move_down(3, PD::M1)->move_down(3, PD::Smp1)->move_down(2, PD::P1)->jump_back();
		
		turtle->move_right(10, PD::t1);
		turtle->move_down(3, PD::T1)->move_down(3, PD::Stp1)->move_down(2, PD::PowerStation1)->jump_back();

		turtle->move_right(5, PD::Stt)->move_right(5, PD::t2);
		turtle->move_down(3, PD::T2)->move_down(3, PD::Stp2)->move_down(2, PD::PowerStation2)->jump_back();

		turtle->move_right(5, PD::g2);
		turtle->move_up(3, PD::G2)->move_up(3, PD::Sgg2)->move_up(3, PD::Generator2)->jump_back();

		turtle->move_right(5, PD::m2);
		turtle->move_down(3, PD::M2)->move_down(3, PD::Smp2)->move_down(2, PD::Generator2)->jump_back();

		turtle->move_right(5, PD::g3);
		turtle->move_up(3, PD::G3)->move_up(3, PD::Sgs)->move_up(3, PD::SolarInverter)->jump_back();

		turtle->move_right(5, PD::b1);
		turtle->move_down(3, PD::B1)->move_down(3, PD::Sbs)->move_down(2, PD::StorageBattery)->jump_back();

		this->diagram = new Tracklet<PD>(turtle, line_thickness, Colours::GhostWhite);

		this->master->insert(this->diagram);

		this->load_graphlets(this->switches, PD::Ssp,  PD::Stt, gridsize, 0.0);
		this->load_graphlets(this->switches, PD::Sgg1, PD::Sbs, gridsize, -90.0);
	}

	void reflow(float width, float height) {
		this->master->move_to(this->diagram, width * 0.5F, height * 0.5F, GraphletAlignment::CC);
		
		for (auto lt = this->captions.begin(); lt != this->captions.end(); lt++) {
			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, GraphletAlignment::CB);
		}

		for (auto lt = this->switches.begin(); lt != this->switches.end(); lt++) {
			this->diagram->map_graphlet_at_anchor(lt->second, lt->first, GraphletAlignment::CC);
		}
	}

private:
	template<class G>
	void load_graphlets(std::map<PD, G*>& gs, PD id0, PD idn, float radius, double degrees) {
		for (PD id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, line_thickness, degrees));
		}
	}

	template<class G>
	void load_graphlets(std::map<PD, G*>& gs, std::map<PD, Labellet*>& ls, PD id0, PD idn, float radius, double degrees) {
		this->load_graphlets(gs, id0, idn, radius, degrees);

		for (PD id = id0; id <= idn; id++) {
			ls[id] = this->make_label(speak(id.ToString()), id, Colours::Silver);
		}
	}

	template<class G>
	void load_graphlets(std::map<PD, G*>& gs, std::map<PD, Labellet*>& ls
		, PD id0, PD idn, float radius, double degrees, std::map<PD, Labellet*>& cs) {
		this->load_graphlets(gs, ls, id0, idn, radius, degrees);

		for (PD id = id0; id <= idn; id++) {
			cs[id] = this->make_label(id, Colours::Silver);
		}
	}

	void load_dimensions(std::map<PD, Dimensionlet*>& sts, PD id0, PD idn, Platform::String^ unit, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr) {
		for (PD id = id0; id <= idn; id++) {
			sts[id] = this->master->insert_one(new Dimensionlet(unit, label, subscript));
		}
	}

	void load_label(std::map<PD, Labellet*>& ls, PD id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->make_label(id, color, font);
	}

	Labellet* make_label(Platform::String^ caption, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		Labellet* label = new Labellet(caption, font, color);

		return this->master->insert_one(label);
	}

	Labellet* make_label(PD id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		return this->make_label(speak(id.ToString()), color, font);
	}

// never deletes these graphlets mannually
private:
	Tracklet<PD>* diagram;
	std::map<PD, Labellet*> captions;
	std::map<PD, Switchlet*> switches;

private:
	PropulsionPage* master;
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
		LineDiagram* diagram = new LineDiagram(this);
		float gridsize = 24.0F;

		diagram->load(gridsize);

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
