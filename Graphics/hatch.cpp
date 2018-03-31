#include "hatch.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float scale_lmark_ratio = 1.0F;

/*
CanvasGeometry^ vhatch(float range, unsigned char step, CanvasTextFormat^ font) {
	TextExtent zero = get_text_extent("0", ((font == nullptr) ? make_text_format(8.0F) : font));
	float ch = zero.width;
	float mark_interval = zero.height * 0.8F;

	{ // make scales and marks
		float linespacing = mark_interval * 2.0F;
		float long_mark_length = ch * scale_lmark_ratio;
		float short_mark_length = long_mark_length * 0.618F;
		float short_mark_x = long_mark_length - short_mark_length;
		float delta = range / step;

		auto marks = blank();
		Platform::String^ scale_sequences = "";
		for (char i = 0; i <= step; i++) {
			float ythis = mark_interval * i;

			if (i % 2 == 0) {
				marks = geometry_union(marks, hline(0.0F, ythis, ch));
				scale_sequences = scale_sequences + " " + (this->range - delta * i).ToString();
			} else {
				marks = geometry_union(marks, hline(short_mark_x, ythis, short_mark_length));
			}
		}

		this->scale_marks = geometry_freeze(marks);
	}
}
*/
