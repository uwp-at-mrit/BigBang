#include "about.hpp"

#include "syslog.hpp"
#include "module.hpp"
#include "tongue.hpp"

#include "datum/string.hpp"
#include "datum/time.hpp"

#include "decorator/background.hpp"
#include "graphlet/textlet.hpp"
#include "graphlet/filesystem/bitmaplet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
namespace {
	private class AboutApplication : public ISatellite {
	public:
		AboutApplication(Platform::String^ logo_name, ICanvasBrush^ bgcolor, ICanvasBrush^ fgcolor, int start_year, Platform::String^ logo_rootdir)
			: ISatellite(Log::Info, __MODULE__) {
			this->push_decorator(new BackgroundDecorator(bgcolor));

			this->fgcolor = fgcolor;
			this->large_font = make_bold_text_format("Microsoft Yahei", 32.0F);
			this->font = make_text_format("Microsoft Yahei", 20.0F);
			this->small_font = make_text_format("Microsoft Yahei", 14.0F);

			this->logo = new Bitmaplet(logo_name, logo_rootdir);
			this->copyright = this->make_label(make_wstring(L"Copyright © %d - %d", start_year, current_year()), this->small_font);
		}

		void fill_extent(float* width, float* height) {
			SET_BOX(width, 400.0F);
			SET_BOX(height, 400.0F);
		}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override {
			this->insert(this->logo);
			this->insert(this->copyright);
			
			this->app_name = this->insert_one(this->make_label(speak(Package::Current->DisplayName), this->large_font));
			this->author = this->insert_one(this->make_label(Package::Current->PublisherDisplayName, this->font));
		}

		void reflow(float width, float height) override {
			float cx = width * 0.5F;

			this->move_to(this->logo, cx, height * 0.25F, GraphletAnchor::CC);
			this->move_to(this->app_name, cx, height * 0.5F, GraphletAnchor::CC);
			this->move_to(this->author, this->app_name, GraphletAnchor::CB, GraphletAnchor::CT);
			this->move_to(this->copyright, this->author, GraphletAnchor::CB, GraphletAnchor::CT);
		}

	private:
		Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
			return this->insert_one(new Labellet(text, font, this->fgcolor));
		}

	private: // never delete these graphlets manually.
		Bitmaplet* logo;
		Labellet* app_name;
		Labellet* author;
		Labellet* copyright;

	private:
		CanvasTextFormat^ large_font;
		CanvasTextFormat^ font;
		CanvasTextFormat^ small_font;
		ICanvasBrush^ fgcolor;
	};
}

/*************************************************************************************************/
static AboutApplication* about = nullptr;

ISatellite* WarGrey::SCADA::make_about(Platform::String^ logo_name, ICanvasBrush^ bgcolor, ICanvasBrush^ fgcolor, int start_year, Platform::String^ logo_rootdir) {
	return new AboutApplication(logo_name, bgcolor, fgcolor, start_year, logo_rootdir);
}

