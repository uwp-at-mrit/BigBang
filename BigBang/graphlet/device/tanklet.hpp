#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"
#include "measure/vhatchmark.hpp"

namespace WarGrey::SCADA {
	private struct TankStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ruler_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ indicator_color;

		/** Note
		 * By design, `mark_weight` that not belongs to [0.0F, 1.0F]
		 * will be considered not being set by client applications.
		 */
		double mark_weight = -1.0;
	};

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
		void prepare_style(WarGrey::SCADA::TankStyle& style, unsigned int idx, unsigned int count);

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

	template<typename State>
	private class Tanklet
		: public WarGrey::SCADA::ITanklet
		, public WarGrey::SCADA::IStatelet<State, WarGrey::SCADA::TankStyle> {
	public:
		Tanklet(float width, float height = 0.0F, float thickness = 3.0F, Platform::String^ tongue = nullptr)
			: Tanklet(_E0(State), width, height, thickness, tongue) {}

		Tanklet(State default_state, float width, float height = 0.0F, float thickness = 3.0F, Platform::String^ tongue = nullptr)
			: ITanklet(width, height, thickness), IStatelet(default_state) {
			for (State s = _E0(State); s < State::_; s ++) {
				this->marks[_I(s)] = speak(s, tongue);
			}
		}

	public:
		void construct() override {
			ITanklet::construct();
		}

	protected:
		void prepare_style(State status, TankStyle& style) {
			ITanklet::prepare_style(style, _I(status), _N(State));
		}

		void apply_style(TankStyle& style) {
			ITanklet::apply_style(&style);
		}

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(float height, float thickness
			, WarGrey::SCADA::VHatchMarkMetrics* metrics) {
			double weights[_N(State)];

			for (State s = _E(State, 0); s < State::_; s++) {
				weights[_I(s)] = this->get_style(s).mark_weight;
			}

			return vrhatchmark(height, this->marks, weights, _N(State), thickness, metrics);
		}

	private:
		Platform::String^ marks[_N(State)];
	};
}
