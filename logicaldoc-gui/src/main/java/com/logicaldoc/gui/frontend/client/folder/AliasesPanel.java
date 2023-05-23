package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FolderAliasesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * This panel shows the aliases of a folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AliasesPanel extends FolderDetailTab {

	private FolderAliasesDS dataSource;

	private ListGrid listGrid;

	public AliasesPanel(final GUIFolder folder) {
		super(folder, null);
	}

	@Override
	protected void onDraw() {
		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);
		ListGridField name = new ListGridField("name", I18N.message("name"), 200);
		ListGridField path = new ListGridField("path", I18N.message("path"));

		ListGridField icon = new ListGridField("icon", " ", 20);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(true);
		listGrid.setAutoFetchData(true);
		dataSource = new FolderAliasesDS(folder.getId());
		listGrid.setDataSource(dataSource);
		listGrid.setFields(id, icon, name, path);
		addMember(listGrid);

		listGrid.addDoubleClickHandler(
				event -> FolderNavigator.get().openFolder(listGrid.getSelectedRecord().getAttributeAsLong("id")));
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}
}