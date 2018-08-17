#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "hatch.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private struct TankStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ indicator_color;

		/** Note
		 * By design, `mark_weight` that not belongs to [0.0F, 1.0F]
		 * will be considered not being set by client applications.
		 */
		float mark_weight = -1.0F; 
	};

	void default_mark_weights(float* weight, unsigned int count);
	void default_liquid_colors(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ lcolors[], unsigned int count);

	private class ITanklet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		ITanklet(float width, float height, float thickness);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(
			float height, float thickness, WarGrey::SCADA::VHatchMarkMetrics* metrics) = 0;

	protected:
		void apply_style(WarGrey::SCADA::TankStyle* style);
		void prepare_style(WarGrey::SCADA::TankStyle& style, unsigned int idx,
			float weights[], Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_colors[]);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ floating;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ indicator;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ tube;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ruler;
		WarGrey::SCADA::TankStyle* style; // do not `delete` it directly 

	private:
		float width;
		float height;
		float thickness;
		
	private:
		float float_y;
		float float_half_height;
		float ruler_em;
	};

	template<typename Status>
	private class Tanklet
		: public WarGrey::SCADA::ITanklet
		, public WarGrey::SCADA::IStatuslet<Status, WarGrey::SCADA::TankStyle> {
	public:
		// WARNING: make sure `weights` and `liquid_colors` has enough elements, or the application may crash at random.
		Tanklet(float width, float height = 0.0F, float thickness = 3.0F, float weights[] = nullptr
			, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_colors[] = nullptr
			, Platform::String^ tongue = nullptr)
			: Tanklet(_E(Status, 0), width, height, thickness, tongue, weights, liquid_colors) {}

		Tanklet(Status default_status, float width, float height = 0.0F, float thickness = 3.0F
			, float weights[] = nullptr, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_colors0[] = nullptr
			, Platform::String^ tongue = nullptr)
			: ITanklet(width, height, thickness), IStatuslet(default_status) {
			unsigned int count = _N(Status);

			for (unsigned int idx = 0; idx < count; idx ++) {
				this->marks[idx] = speak(_E(Status, idx), tongue);
				this->weights[idx] = ((weights == nullptr) ? -1.0F : weights[idx]);
				this->liquid_colors[idx] = ((liquid_colors == nullptr) ? nullptr : liquid_colors[idx]);
			}

			if (weights == nullptr) {
				WarGrey::SCADA::default_mark_weights(this->weights, count);
			}

			if (liquid_colors == nullptr) {
				WarGrey::SCADA::default_liquid_colors(this->liquid_colors, count);
			}
		}

	public:
		void construct() override {
			ITanklet::construct();
			this->update_status();
		}

	protected:
		void prepare_style(Status status, TankStyle& style) {
			ITanklet::prepare_style(style, _I(status), this->weights, this->liquid_colors);
		}

		void apply_style(TankStyle& style) {
			ITanklet::apply_style(&style);
		}

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(float height, float thickness
			, WarGrey::SCADA::VHatchMarkMetrics* metrics) {
			return vrhatchmark(height, this->marks, this->weights, _N(Status), thickness, metrics);
		}

	private:
		float weights[_N(Status)];
		Platform::String^ marks[_N(Status)];
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_colors[_N(Status)];
	};
}
