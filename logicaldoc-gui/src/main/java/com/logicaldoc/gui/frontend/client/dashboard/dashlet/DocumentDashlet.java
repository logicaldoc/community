package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
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
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Portlet specialized in listing documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 6.0
 */
public class DocumentDashlet extends Dashlet {

	protected RefreshableListGrid list;

	protected int status;

	protected HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE,
			event -> GridUtil.exportCSV(list, true));

	protected HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, event -> GridUtil.print(list));

	public DocumentDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);

		String icn = "file";
		String title = I18N.message(guiDashlet.getTitle());

		if ("checkout".equals(guiDashlet.getName())) {
			icn = "edit";
		} else if ("locked".equals(guiDashlet.getName())) {
			icn = "lock-alt";
		}

		setTitle(AwesomeFactory.getIconHtml(icn, title));

		exportControl.setTooltip(I18N.message("export"));
		printControl.setTooltip(I18N.message("print"));

		setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, exportControl, printControl,
				HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
	}

	private List<String> getColumnsList() {
		List<String> set = guiDashlet.getColumnsList();
		if (set == null || set.isEmpty()) {
			set = new ArrayList<>();
			set.add("filename");
			set.add("version");
			set.add("published");
		}
		return set;
	}

	protected RefreshableListGrid getListGrid() {
		return new DocumentsListGrid(guiDashlet.getExtendedAttributes());
	}

	public String getDocIdAttribute() {
		return "id";
	}

	public void prepareClickHandlers(RefreshableListGrid ret, String docIdAttribute) {
		ret.addCellContextClickHandler(event -> {
			event.cancel();
			Record rec = event.getRecord();
			DocumentService.Instance.get().getById(Long.parseLong(rec.getAttributeAsString(docIdAttribute)),
					new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument document) {
							Menu contextMenu = prepareContextMenu(document);
							contextMenu.showContextMenu();
						}
					});
		});

		ret.addCellDoubleClickHandler(event -> {
			Record rec = event.getRecord();
			if (rec.getAttribute("folderId") != null)
				DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString("folderId")),
						Long.parseLong(rec.getAttributeAsString(docIdAttribute)));
			else
				DocumentsPanel.get().openInFolder(Long.parseLong(rec.getAttributeAsString(docIdAttribute)));
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

		list.setFields(prepareGridFields(list).toArray(new ListGridField[0]));

		prepareClickHandlers(list, getDocIdAttribute());

		addItem(list);
	}

	protected List<ListGridField> prepareGridFields(RefreshableListGrid grid) {
		List<ListGridField> fields = new ArrayList<>();

		Map<String, ListGridField> fieldsMap = ((DocumentsListGrid) grid).getFieldsMap();
		fieldsMap.get("statusIcons").setHidden(true);

		fields.add(fieldsMap.get(getDocIdAttribute()));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		for (String col : getColumnsList()) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}

		((DocumentsListGrid) grid).mergeFields(fields);

		return fields;
	}

	@Override
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = super.prepareContextMenu(document);
		if (document.getStatus() == Constants.DOC_LOCKED || document.getStatus() == Constants.DOC_CHECKED_OUT) {
			MenuItem unlock = new MenuItem();
			unlock.setTitle(I18N.message("unlock"));
			unlock.addClickHandler(event -> {
				DocumentService.Instance.get().unlock(new long[] { document.getId() }, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						Session.get().getUser().setLockedDocs(Session.get().getUser().getLockedDocs() - 1);
						list.removeSelectedData();
						list.refresh(getDataSource());
					}
				});
			});
			contextMenu.addItem(unlock);
		}
		return contextMenu;
	}

	@Override
	protected void refresh() {
		list.refresh(getDataSource());
	}

	protected DataSource getDataSource() {
		return new DocumentsDS(getDataSourceUrl(), I18N.getLocale(), guiDashlet.getExtendedAttributes());
	}
}