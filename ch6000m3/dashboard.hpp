#pragma once

#include <map>

#include "credit.hpp"
#include "module.hpp"
#include "brushes.hxx"
#include "planet.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/booleanlet.hpp"

namespace WarGrey::SCADA {
	template<class T, typename E>
	private class DashBoard abstract {
	public:
		DashBoard(T* master, Platform::String^ src) : master(master), scope(module_name(src)) {}

	public:
		template<class G>
		void load_graphlets(std::map<E, G*>& gs, E id0, E idn, float radius, double degrees) {
			for (E id = id0; id <= idn; id++) {
				gs[id] = this->master->insert_one(new G(radius, degrees));
				gs[id]->id = id;
			}
		}

		template<class G>
		void load_graphlets(std::map<E, G*>& gs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, E id0, E idn, float radius, double degrees) {
			this->load_graphlets(gs, id0, idn, radius, degrees);
			
			for(E id = id0; id <= idn; id++) {
			    ls[id] = this->make_label(speak(id, this->scope), id, WarGrey::SCADA::Colours::Silver);
		    }
	    }

		template<class G>
		void load_graphlets(std::map<E, G*>& gs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
			this->load_graphlets(gs, id0, idn, radius, degrees);

			for (E id = id0; id <= idn; id++) {
				ls[id] = this->make_label(id.ToString(), id, WarGrey::SCADA::Colours::Silver);
				cs[id] = this->make_label(id, WarGrey::SCADA::Colours::Silver);
			}
		}

		void load_dimension(std::map<E, Credit<WarGrey::SCADA::Dimensionlet, E>*>& ds, E id
			, Platform::String^ unit, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, subscript));
			ds[id]->id = id;
		}

		void load_dimensions(std::map<E, Credit<WarGrey::SCADA::Dimensionlet, E>*>& ds, E id0, E idn
			, Platform::String^ unit, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr) {
			for(E id = id0; id <= idn; id++) {
				this->load_dimension(ds, id, unit, label, subscript);
			}
		}

		void load_label(std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls, Platform::String^ caption, E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			ls[id] = this->make_label(caption, id, color, font);
		}

		void load_label(std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls, E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			ls[id] = this->make_label(id, color, font);
		}

		void load_status_indicator(E id, float size
			, std::map<E, Credit<WarGrey::SCADA::Booleanlet, E>*>& bs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color) {
			ls[id] = this->make_label(speak(id, this->scope), id, WarGrey::SCADA::Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<WarGrey::SCADA::Booleanlet, E>(size, color));
			bs[id]->id = id;
		}

	public:
		Credit<WarGrey::SCADA::Labellet, E>* make_label(Platform::String^ caption, E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			Credit<Labellet, E>* label = new Credit<Labellet, E>(caption, font, color);

			label->id = id;

			return this->master->insert_one(label);
		}

		Credit<WarGrey::SCADA::Labellet, E>* make_label(E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			return this->make_label(speak(id, this->scope), id, color, font);
		}

	protected:
		T* master;

	private:
		Platform::String^ scope;
	};
}
