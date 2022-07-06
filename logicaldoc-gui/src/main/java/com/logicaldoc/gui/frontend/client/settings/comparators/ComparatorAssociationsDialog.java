package com.logicaldoc.gui.frontend.client.settings.comparators;

import com.logicaldoc.gui.common.client.data.ComparatorsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * This is the dialog used to quickly associate a comparator to formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class ComparatorAssociationsDialog extends Window {

	private static final String DEFAULT_COMPARATOR = "com.logicaldoc.comparison.basic.BasicComparator";

	private ListGrid associationsGrid;

	private SelectItem comparator;

	private IButton apply;

	// The grid that maintains the associations
	private ListGrid srcGrid;

	public ComparatorAssociationsDialog(final ListGrid srcGrid) {
		this.srcGrid = srcGrid;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("associations"));
		setWidth(320);
		setHeight(360);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		comparator = ItemFactory.newComparatorSelector();
		comparator.setWidth(290);
		comparator.setRequired(true);
		comparator.setValue(DEFAULT_COMPARATOR);
		comparator.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(comparator);

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
		associationsGrid.setSelectionType(SelectionStyle.SIMPLE);
		associationsGrid.setWidth100();
		associationsGrid.setHeight("*");
		associationsGrid.setSelectionAppearance(SelectionAppearance.CHECKBOX);
		associationsGrid.setSelectionProperty("selected");
		ListGridField in = new ListGridField("in", I18N.message("ext"), 40);
		in.setCanEdit(false);

		associationsGrid.setFields(in);

		ComparatorsDS ds = new ComparatorsDS(null);
		ds.setComparator(comparator.getValueAsString() != null ? comparator.getValueAsString() : DEFAULT_COMPARATOR);
		associationsGrid.setDataSource(ds);
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
				String selectedComparator = comparator.getValueAsString();

				Record record = srcGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS, id));
				if (record != null) {
					record.setAttribute("comparator", selectedComparator);
					srcGrid.updateData(record);
				}
			}
		}
		destroy();
	}
}