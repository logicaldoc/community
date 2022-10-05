package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import java.util.ArrayList;
import java.util.HashMap;
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
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

public class DocumentDashlet extends Dashlet {

	protected DocumentsListGrid list;

	protected int status;

	// Stores all the possible fields we can use in a grid of documents
	protected Map<String, ListGridField> fieldsMap = new HashMap<String, ListGridField>();

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

		initGUI();
	}

	private List<String> getColumnsList() {
		List<String> set = guiDashlet.getColumnsList();
		if (set == null || set.isEmpty()) {
			set = new ArrayList<String>();
			set.add("filename");
			set.add("version");
			set.add("published");
		}
		return set;
	}

	private void initGUI() {
		list = new DocumentsListGrid(guiDashlet.getExtendedAttributes());
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

		Map<String, ListGridField> fieldsMap = list.getFieldsMap();
		fieldsMap.get("statusIcons").setHidden(true);

		final List<ListGridField> fields = new ArrayList<ListGridField>();

		fields.add(fieldsMap.get("id"));
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

		if (!fields.contains(fieldsMap.get("filename"))) {
			fieldsMap.get("filename").setHidden(true);
			fields.add(fieldsMap.get("filename"));
		}
		if (!fields.contains(fieldsMap.get("lastModified"))) {
			fieldsMap.get("lastModified").setHidden(true);
			fields.add(fieldsMap.get("lastModified"));
		}
		if (!fields.contains(fieldsMap.get("type"))) {
			fieldsMap.get("type").setHidden(true);
			fields.add(fieldsMap.get("type"));
		}
		if (!fields.contains(fieldsMap.get("size"))) {
			fieldsMap.get("size").setHidden(true);
			fields.add(fieldsMap.get("size"));
		}
		if (!fields.contains(fieldsMap.get("pages"))) {
			fieldsMap.get("pages").setHidden(true);
			fields.add(fieldsMap.get("pages"));
		}
		if (!fields.contains(fieldsMap.get("fileVersion"))) {
			fieldsMap.get("fileVersion").setHidden(true);
			fields.add(fieldsMap.get("fileVersion"));
		}
		if (!fields.contains(fieldsMap.get("version"))) {
			fieldsMap.get("version").setHidden(true);
			fields.add(fieldsMap.get("version"));
		}
		if (!fields.contains(fieldsMap.get("publisher"))) {
			fieldsMap.get("publisher").setHidden(true);
			fields.add(fieldsMap.get("publisher"));
		}
		if (!fields.contains(fieldsMap.get("published"))) {
			fieldsMap.get("published").setHidden(true);
			fields.add(fieldsMap.get("published"));
		}
		if (!fields.contains(fieldsMap.get("creator"))) {
			fieldsMap.get("creator").setHidden(true);
			fields.add(fieldsMap.get("creator"));
		}
		if (!fields.contains(fieldsMap.get("created"))) {
			fieldsMap.get("created").setHidden(true);
			fields.add(fieldsMap.get("created"));
		}
		if (!fields.contains(fieldsMap.get("customId"))) {
			fieldsMap.get("customId").setHidden(true);
			fields.add(fieldsMap.get("customId"));
		}
		if (!fields.contains(fieldsMap.get("rating"))) {
			fieldsMap.get("rating").setHidden(true);
			fields.add(fieldsMap.get("rating"));
		}
		if (!fields.contains(fieldsMap.get("comment"))) {
			fieldsMap.get("comment").setHidden(true);
			fields.add(fieldsMap.get("comment"));
		}
		if (!fields.contains(fieldsMap.get("workflowStatus"))) {
			fieldsMap.get("workflowStatus").setHidden(true);
			fields.add(fieldsMap.get("workflowStatus"));
		}
		if (!fields.contains(fieldsMap.get("template"))) {
			fieldsMap.get("template").setHidden(true);
			fields.add(fieldsMap.get("template"));
		}
		if (!fields.contains(fieldsMap.get("startPublishing"))) {
			fieldsMap.get("startPublishing").setHidden(true);
			fields.add(fieldsMap.get("startPublishing"));
		}
		if (!fields.contains(fieldsMap.get("stopPublishing"))) {
			fieldsMap.get("stopPublishing").setHidden(true);
			fields.add(fieldsMap.get("stopPublishing"));
		}
		if (!fields.contains(fieldsMap.get("language"))) {
			fieldsMap.get("language").setHidden(true);
			fields.add(fieldsMap.get("language"));
		}

		list.setFields(fields.toArray(new ListGridField[0]));

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				event.cancel();
				Record record = event.getRecord();
				DocumentService.Instance.get().getById(Long.parseLong(record.getAttributeAsString("id")),
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
			}
		});

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("id")));
			}
		});

		addItem(list);

		HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});
		exportControl.setTooltip(I18N.message("export"));

		HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});
		printControl.setTooltip(I18N.message("print"));

		setHeaderControls(HeaderControls.HEADER_LABEL, refreshControl, exportControl, printControl,
				HeaderControls.MINIMIZE_BUTTON, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
	}

	private DocumentsDS getDataSource() {
		return new DocumentsDS(getDataSourceUrl(), I18N.getLocale(), guiDashlet.getExtendedAttributes());
	}

	@Override
	protected Menu prepareContextMenu(final GUIDocument document) {
		Menu contextMenu = super.prepareContextMenu(document);
		if (document.getStatus() == Constants.DOC_LOCKED || document.getStatus() == Constants.DOC_CHECKED_OUT) {
			MenuItem unlock = new MenuItem();
			unlock.setTitle(I18N.message("unlock"));
			unlock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
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
				}
			});
			contextMenu.addItem(unlock);
		}
		return contextMenu;
	}

	@Override
	protected void refresh() {
		list.refresh(getDataSource());
	}
}