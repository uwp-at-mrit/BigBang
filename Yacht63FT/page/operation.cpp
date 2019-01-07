#include "page/operation.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum class YachtSkeleton { Upper, Bottom, _ };

/*************************************************************************************************/
private class OpBoard final : public PLCConfirmation {
public:
	OpBoard(OperationPage* master) : master(master) {
		this->font = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(33.75F));
	}

public:
	void load_and_flow(float width, float height) {
	}

	void load_skeleton(float width, float height) {
		this->skeleton = this->master->insert_one(new UnionBitmaplet<YachtSkeleton>("Yacht"),
			width * 0.5F, height * 0.5F, GraphletAnchor::CC);
	}

// never deletes these graphlets mannually
private:
	UnionBitmaplet<YachtSkeleton>* skeleton;
		
private:
	CanvasTextFormat^ font;
	OperationPage* master;
};

/*************************************************************************************************/
OperationPage::OperationPage(PLCMaster* device, Platform::String^ name) : Planet(name), device(device) {}

OperationPage::~OperationPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void OperationPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		OpBoard* ob = new OpBoard(this);

		ob->load_skeleton(width, height);
		ob->load_and_flow(width, height);

		this->dashboard = ob;
		this->device->push_confirmation_receiver(ob);
	}
}

void OperationPage::on_tap(IGraphlet* g, float local_x, float local_y) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
