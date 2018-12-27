#include <map>

#include "plc.hpp"
#include "widget/settings.hpp"
#include "configuration.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/shapelet.hpp"

#include "system.hpp"
#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

/*************************************************************************************************/
private class Settings : public ISatellite, public PLCConfirmation {
public:
	Settings(PLCMaster* device) : ISatellite(default_logging_level, __MODULE__) {}

	void Settings::fill_satellite_extent(float* width, float* height) {
		float margin = normal_font_size * 4.0F;
		Size size = system_screen_size();

		SET_BOX(width, size.Width - margin);
		SET_BOX(height, size.Height - margin);
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
	}

	void reflow(float width, float height) override {
	}

public:
	bool Settings::can_select(IGraphlet* g) override {
		return false;
	}

private:
	Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
		return this->insert_one(new Labellet(text, font));
	}

	template<typename T, typename S>
	void load_remote_primitive(T** g, S s, float unitsize) {
		(*g) = this->insert_one(new T(s, unitsize));
		(*g)->set_remote_control(true);
	}

	template<typename T, typename S>
	void load_primitives(std::map<S, T*>& gs, std::map<S, Labellet*>& ls, float unitsize) {
		for (S s = _E0(S); s < S::_; s++) {
			gs[s] = this->insert_one(new T(s, unitsize));
			ls[s] = make_label(_speak(s), this->status_font);
		}
	}

	template<typename T, typename S>
	void reflow_primitives(std::map<S, T*>& gs, std::map<S, Labellet*>& ls
		, float x0, float* y, float halfunit, float cellsize) {
		this->reflow_primitives(gs, ls, _E0(S), S::_, x0, y, halfunit, cellsize);
	}

	template<typename T, typename S>
	void reflow_primitives(std::map<S, T*>& gs, std::map<S, Labellet*>& ls
		, S id0, S idN, float x0, float* y, float halfunit, float cellsize) {
		unsigned int i0 = _I(id0);

		for (S i = id0; i < idN; i++) {
			float x = x0 + float(_I(i) - i0) * cellsize;

			this->move_to(gs[i], x, (*y) + halfunit * 1.0F, GraphletAnchor::CC);
			this->move_to(ls[i], x, (*y) + halfunit * 2.0F, GraphletAnchor::CT);
		}

		(*y) = (*y) + cellsize;
	}

private: // never delete these graphlets manually.
	//Labellet* captions[_N(GS)];

private:
	CanvasTextFormat^ font;
	CanvasTextFormat^ status_font;

private:
	PLCMaster* device;
};

/*************************************************************************************************/
ISatellite* WarGrey::SCADA::make_settings(PLCMaster* plc) {
	return new Settings(plc);
}
