#pragma once
#pragma warning(disable: 4250)

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"
#include "measure/vhatchmark.hpp"

namespace WarGrey::SCADA {
	private enum class TankState { Full, UltraHigh, High, Normal, Low, UltraLow, Empty, _ };

	private struct TankStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ruler_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ indicator_color;
	};

	private class ITanklet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		ITanklet(float width, float height, float thickness);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual WarGrey::SCADA::TankStyle* get_tank_style() = 0;
		virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(
			float height, float thickness, WarGrey::SCADA::VHatchMarkMetrics* metrics) = 0;

	protected:
		void update_level(double value, double vmin, double vmax);
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ floating;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ indicator;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ tube;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ruler;

	private:
		float width;
		float height;
		float thickness;

	private:
		float float_y;
		float float_half_height;
		float ruler_em;
	};

	private class Tanklet
		: public WarGrey::SCADA::ITanklet
		, public WarGrey::SCADA::IStatelet<WarGrey::SCADA::TankState, WarGrey::SCADA::TankStyle>
		, public WarGrey::SCADA::IRangelet<double> {
	public:
		Tanklet(double range, float width, float height = 0.0F, unsigned int step = 0U,
			float thickness = 3.0F, unsigned int precision = 0U);

		Tanklet(WarGrey::SCADA::TankState default_state, double range, float width, float height = 0.0F,
			unsigned int step = 0U, float thickness = 3.0F, unsigned int precision = 0U);

		Tanklet(double vmin, double vmax, float width, float height = 0.0F, unsigned int step = 0U,
			float thickness = 3.0F, unsigned int precision = 0U);

		Tanklet(WarGrey::SCADA::TankState default_state, double vmin, double vmax, float width, float height = 0.0F,
			unsigned int step = 0U, float thickness = 3.0F, unsigned int precision = 0U);

	protected:
		void prepare_style(WarGrey::SCADA::TankState state, WarGrey::SCADA::TankStyle& style) override;
		void on_value_changed(double v) override;

	protected:
		WarGrey::SCADA::TankStyle* get_tank_style() override;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(float height, float thickness,
			WarGrey::SCADA::VHatchMarkMetrics* metrics) override;

	private:
		unsigned int step;
		unsigned int precision;
	};

	/************************************************************************************************/
	// TODO: find a better name for Tanks that have self-defined states and no liquid level info.
	private struct StateTankStyle : public WarGrey::SCADA::TankStyle {
		/** Note
		 * By design, `mark_weight` that not belongs to [0.0F, 1.0F]
		 * will be considered not being set by client applications.
		 */
		double mark_weight = -1.0;
	};

	private class IStateTanklet abstract : public WarGrey::SCADA::ITanklet {
	public:
		IStateTanklet(float width, float height, float thickness);

	protected:
		void prepare_style(WarGrey::SCADA::StateTankStyle& style, unsigned int idx, unsigned int count);
	};

	template<typename State>
	private class StateTanklet
		: public WarGrey::SCADA::IStateTanklet
		, public WarGrey::SCADA::IStatelet<State, WarGrey::SCADA::StateTankStyle> {
	public:
		StateTanklet(float width, float height = 0.0F, float thickness = 3.0F, Platform::String^ tongue = nullptr)
			: StateTanklet(_E0(State), width, height, thickness, tongue) {}

		StateTanklet(State default_state, float width, float height = 0.0F, float thickness = 3.0F, Platform::String^ tongue = nullptr)
			: IStateTanklet(width, height, thickness), IStatelet(default_state) {
			for (State s = _E0(State); s < State::_; s ++) {
				this->marks[_I(s)] = speak(s, tongue);
			}
		}

	protected:
		void prepare_style(State state, StateTankStyle& style) {
			IStateTanklet::prepare_style(style, _I(state), _N(State));
		}

		void on_state_changed(State state) {
			IStateTanklet::update_level(this->get_style().mark_weight, 0.0, 1.0);
		}

		WarGrey::SCADA::TankStyle* get_tank_style() override {
			return &(this->get_style());
		}

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_ruler(float height, float thickness
			, WarGrey::SCADA::VHatchMarkMetrics* metrics) override {
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
