package com.logicaldoc.gui.frontend.client.system;

import com.logicaldoc.gui.common.client.data.EnvironmentDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the list of environment variables
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class EnvironmentPanel extends VLayout {

	private static final String VALUE = "value";

	private static final String SCOPE = "scope";

	public EnvironmentPanel() {
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		ListGridField name = new ListGridField("name", I18N.message("setting"), 210);
		name.setCanFilter(true);
		name.setCanSort(true);

		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE), 280);
		value.setCanFilter(true);
		value.setCanSort(true);

		ListGridField scope = new ListGridField(SCOPE, I18N.message(SCOPE), 80);
		scope.setCanFilter(true);
		scope.setCanSort(true);

		RefreshableListGrid grid = new RefreshableListGrid();
		grid.setWidth100();
		grid.setHeight100();
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setCanExpandRecords(false);
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);
		grid.setCanFreezeFields(true);
		grid.setAutoFetchData(true);
		grid.setSelectionType(SelectionStyle.MULTIPLE);
		grid.setFilterOnKeypress(true);
		grid.setShowFilterEditor(true);
		grid.setDataSource(new EnvironmentDS());
		grid.setFields(name, value, scope);
		grid.addDoubleClickHandler(event -> {
			LD.askForValue(I18N.message(VALUE), I18N.message(VALUE),
					grid.getSelectedRecord().getAttributeAsString("name") + " = "
							+ grid.getSelectedRecord().getAttributeAsString(VALUE),
					350, val -> {
						// Nothing to do
					});
			event.cancel();
		});

		setMembers(grid);
	}
}