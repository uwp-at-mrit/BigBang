#pragma once

#include <map>

#include "credit.hpp"
#include "brushes.hxx"
#include "planet.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/booleanlet.hpp"

namespace WarGrey::SCADA {
	template<class T, typename E>
	private class Console abstract {
	public:
		Console(T* master, Platform::String^ l10n) : master(master), l10n_prefix(l10n) {}

	public:
		template<class G>
		void load_graphlets(std::map<E, G*>& gs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, E id0, E idn, float radius, double degrees) {
		    for(E id = id0; id <= idn; id++) {
			    gs[id] = this->master->insert_one(new G(radius, degrees));
				gs[id]->id = id;
				ls[id] = this->make_label(speak(id.ToString()), id, Colours::Silver);
		    }
	    }

		template<class G>
		void load_graphlets(std::map<E, G*>& gs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, E id0, E idn, float radius, double degrees, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& cs) {
			this->load_graphlets(gs, ls, id0, idn, radius, degrees);

			for(E id = id0; id <= idn; id++) {
				cs[id] = this->make_label(id, Colours::Silver);
			}
		}

		void load_scales(std::map<E, Credit<WarGrey::SCADA::ScaleTextlet, E>*>& sts, E id0, E idn
			, Platform::String^ unit, Platform::String^ label = nullptr, Platform::String^ subscript = nullptr) {
			for(E id = id0; id <= idn; id++) {
				sts[id] = this->master->insert_one(new Credit<ScaleTextlet, HS>(unit, label, subscript));
				sts[id]->id = id;
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

		void load_state_indicator(E id, float size
			, std::map<E, Credit<WarGrey::SCADA::Booleanlet, E>*>& bs, std::map<E, Credit<WarGrey::SCADA::Labellet, E>*>& ls
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color) {
			ls[id] = this->make_label(speak(id.ToString()), id, WarGrey::SCADA::Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<WarGrey::SCADA::Booleanlet, E>(size, color));
			bs[id]->id = id;
		}

		Credit<WarGrey::SCADA::Labellet, E>* make_label(Platform::String^ caption, E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			Credit<Labellet, E>* label = new Credit<Labellet, E>(caption);

			label->id = id;
			label->set_color(color);

			if (font != nullptr) {
				label->set_font(font);
			}

			return this->master->insert_one(label);
		}

		Credit<WarGrey::SCADA::Labellet, E>* make_label(E id
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color
			, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr) {
			Platform::String^ idstr = id.ToString();
			Platform::String^ caption = (this->l10n_prefix == nullptr ? idstr : this->l10n_prefix + "_" + idstr);

			return this->make_label(speak(caption), id, color, font);
		}

	protected:
		T* master;

	protected:
		Platform::String^ l10n_prefix;
	};
}