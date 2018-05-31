#pragma once

#include "graphlet/primitive.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class IDataProvider abstract : public WarGrey::SCADA::SharedObject {
	public:
		//virtual unsigned int column_count() = 0;
		//virtual const char* column_name(unsigned int col_idx) = 0;

	public:
		//virtual unsigned int record_count() = 0;
		//virtual const char* cell_content(unsigned int row_idx, unsigned int col_idx) = 0;

	protected:
		virtual ~IDataProvider() noexcept {}
	};

	private class IDataViewlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		virtual ~IDataViewlet() noexcept;
		IDataViewlet(WarGrey::SCADA::IDataProvider* datasource, float width = 0.0F, float height = 0.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	protected:
		WarGrey::SCADA::IDataProvider* datasource;

	private:
		float width;
		float height;
	};

	private class ListViewlet : public WarGrey::SCADA::IDataViewlet {
	public:
		ListViewlet(WarGrey::SCADA::IDataProvider* datasource, float width = 0.0F, float height = 0.0F);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
	};
}
