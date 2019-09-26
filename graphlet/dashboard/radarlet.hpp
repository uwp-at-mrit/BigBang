#pragma once

#include "graphlet/primitive.hpp"

#include "datum/time.hpp"
#include "datum/object.hpp"

#include "tongue.hpp"
#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private struct RadarStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ outline_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ shape_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ axes_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color;

		float outline_thickness = 1.5F;
	};

	/************************************************************************************************/
	private class IRadarlet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		virtual ~IRadarlet() noexcept;
		IRadarlet(float radius, unsigned int count, float* initials = nullptr);
		IRadarlet(float radius, unsigned int count, WarGrey::SCADA::RadarStyle& style, float* initials = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_scales(float* vs);
		void set_scale(unsigned int index, float v);

	protected:
		Platform::Array<Platform::String^>^ captions;

	private:
		void prepare_style();

	private:
		void reshape();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ caption;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape;

	private:
		WarGrey::SCADA::RadarStyle style;

	private:
		unsigned int count;
		float* scales;
		float Radius;
		float radius;
	};

	template<typename E>
	private class Radarlet : public WarGrey::SCADA::IRadarlet {
	public:
		Radarlet(Platform::String^ tongue, float radius, float* initials = nullptr)
			: IRadarlet(radius, _N(E), initials) {
			this->initialize_caption(tongue);
		}

		Radarlet(Platform::String^ tongue, float radius, WarGrey::SCADA::RadarStyle& style, float* initials = nullptr)
			: IRadarlet(radius, _N(E), style, initials) {
			this->initialize_caption(tongue);
		}

	public:
		void set_scale(E id, float v) {
			IRadarlet::set_scale(_I(id), v);
		}

	private:
		void initialize_caption(Platform::String^ tongue) {
			for (E id = _E0(E); id < E::_; id++) {
				this->captions[_I(id)] = speak(id, tongue);
			}
		}
	};
}
