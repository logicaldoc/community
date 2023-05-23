package com.logicaldoc.gui.frontend.client.impex.converters;

import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorAssociationsDialog;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * This is the dialog used to quickly associate a converter to formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ConverterAssociationsDialog extends ComparatorAssociationsDialog {

	public ConverterAssociationsDialog(final ListGrid srcGrid) {
		super(srcGrid);
		comparatorAttributeName = "converter";
	}

	@Override
	protected void prepareSelectItem() {
		selectItem = ItemFactory.newFormatConverterSelector();
		defaultComparator = "com.logicaldoc.conversion.LibreOfficeConverter";
	}

	@Override
	protected void refresh() {
		if (associationsGrid != null) {
			removeItem(associationsGrid);
		}

		if (apply != null) {
			removeItem(apply);
		}

		associationsGrid = new ListGrid();
		associationsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		associationsGrid.setShowFilterEditor(true);
		associationsGrid.setFilterOnKeypress(true);
		associationsGrid.setAutoFetchData(true);
		associationsGrid.setEditByCell(true);
		associationsGrid.setSelectionType(SelectionStyle.SIMPLE);
		associationsGrid.setWidth100();
		associationsGrid.setHeight("*");
		associationsGrid.setSelectionAppearance(SelectionAppearance.CHECKBOX);
		associationsGrid.setSelectionProperty("selected");
		ListGridField in = new ListGridField("in", I18N.message("in"), 40);
		in.setCanEdit(false);
		ListGridField out = new ListGridField("out", I18N.message("out"), 40);
		out.setCanEdit(false);

		associationsGrid.setFields(in, out);

		if (selectItem.getValueAsString() != null)
			associationsGrid.setDataSource(new FormatConvertersDS(selectItem.getValueAsString()));
		else
			associationsGrid.setDataSource(new FormatConvertersDS(defaultComparator));

		apply = new IButton(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(event -> onApply());

		addItem(associationsGrid);
		addItem(apply);
	}
}