package com.logicaldoc.gui.frontend.client.impex.converters;

import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorAssociationsDialog;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * This is the dialog used to quickly associate a converter to formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ConverterAssociationsDialog extends ComparatorAssociationsDialog {

	private static final String DEFAULT_CONVERTER = "com.logicaldoc.conversion.LibreOfficeConverter";

	private ListGrid associationsGrid;

	private SelectItem converter;

	private IButton apply;

	// The grid that maintains the associations
	private ListGrid srcGrid;

	public ConverterAssociationsDialog(final ListGrid srcGrid) {
		super(srcGrid);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("associations"));
		setWidth(320);
		setHeight(360);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		converter = ItemFactory.newFormatConverterSelector();
		converter.setWidth(290);
		converter.setRequired(true);
		converter.setValue(DEFAULT_CONVERTER);
		converter.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(converter);

		addItem(form);

		refresh();
	}

	private void refresh() {
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
//		associationsGrid.setAlwaysShowEditors(true);
		associationsGrid.setSelectionType(SelectionStyle.SIMPLE);
//		associationsGrid.setEditEvent(ListGridEditEvent.CLICK);
		associationsGrid.setWidth100();
		associationsGrid.setHeight("*");
		associationsGrid.setSelectionAppearance(SelectionAppearance.CHECKBOX);
		associationsGrid.setSelectionProperty("selected");
		ListGridField in = new ListGridField("in", I18N.message("in"), 40);
		in.setCanEdit(false);
		ListGridField out = new ListGridField("out", I18N.message("out"), 40);
		out.setCanEdit(false);

		associationsGrid.setFields(in, out);

		if (converter.getValueAsString() != null)
			associationsGrid.setDataSource(new FormatConvertersDS(converter.getValueAsString()));
		else
			associationsGrid.setDataSource(new FormatConvertersDS(DEFAULT_CONVERTER));

		apply = new IButton(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onApply();
			}
		});

		addItem(associationsGrid);
		addItem(apply);
	}

	protected void onApply() {
		for (Record rec : associationsGrid.getRecordList().toArray()) {
			if (rec.getAttributeAsBoolean("selected")) {
				String id = rec.getAttributeAsString("id").trim();
				String selectedConverter = converter.getValueAsString();

				Record record = srcGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS, id));
				if (record != null) {
					record.setAttribute("converter", selectedConverter);
					srcGrid.updateData(record);
				}
			}
		}
		destroy();
	}
}