package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.BookmarksDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.menu.Menu;

/**
 * Portlet specialized in showing the bookmarks
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.9
 */
public class BookmarkDashlet extends Dashlet {

	private static final String TARGET_ID = "targetId";

	protected RefreshableListGrid list;

	protected int status;

	protected HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE,
			event -> GridUtil.exportCSV(list, true));

	protected HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, event -> GridUtil.print(list));

	public BookmarkDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);

		String icn = "bookmark";
		String title = I18N.message(guiDashlet.getTitle());

		setTitle(AwesomeFactory.getIconHtml(icn, title));

		exportControl.setTooltip(I18N.message("export"));
		printControl.setTooltip(I18N.message("print"));

		setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, exportControl, printControl,
				HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
	}

	protected RefreshableListGrid getListGrid() {
		return new DocumentsListGrid(guiDashlet.getExtendedAttributes());
	}

	public void prepareClickHandlers(RefreshableListGrid ret, String docIdAttribute) {
		ret.addCellContextClickHandler(event -> {
			event.cancel();
			Record rec = event.getRecord();
			DocumentService.Instance.get().getById(Long.parseLong(rec.getAttributeAsString(docIdAttribute)),
					new DefaultAsyncCallback<>() {

						@Override
						public void onSuccess(GUIDocument document) {
							Menu contextMenu = prepareContextMenu(document);
							contextMenu.showContextMenu();
						}
					});
		});

		ret.addCellDoubleClickHandler(event -> {
			Record rec = event.getRecord();
			if (!"folder".equals(rec.getAttribute("icon")))
				DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("folderId"),
						rec.getAttributeAsLong(TARGET_ID));
			else
				DocumentsPanel.get().openInFolder(rec.getAttributeAsLong(TARGET_ID), null);
		});
	}

	@Override
	protected void onDraw() {
		if (list != null)
			removeItem(list);

		list = getListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setCanGroupBy(false);
		list.setCanReorderFields(false);
		list.setCanFreezeFields(false);
		list.setSelectionType(SelectionStyle.NONE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(getDataSource());

		ListGridField id = new ColoredListGridField("id", "id");
		id.setHidden(true);
		id.setAutoFitWidth(true);
		
		ListGridField name = new FileNameListGridField("name", "icon");
		name.setTitle(I18N.message("name"));
		name.setAutoFitWidth(true);
		name.setMinWidth(250);
		
		ListGridField description = new ColoredListGridField("description", I18N.message("description"));
		description.setWidth("*");
		
		ListGridField targetId = new ColoredListGridField(TARGET_ID, I18N.message("targetid"));
		targetId.setHidden(true);
		targetId.setAutoFitWidth(true);

		FileNameListGridField filename = new FileNameListGridField();
		filename.setAutoFitWidth(true);

		list.setFields(id, targetId, name, description);

		prepareClickHandlers(list, TARGET_ID);

		addItem(list);
	}

	@Override
	protected void refresh() {
		list.refresh(getDataSource());
	}

	protected DataSource getDataSource() {
		return new BookmarksDS();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}