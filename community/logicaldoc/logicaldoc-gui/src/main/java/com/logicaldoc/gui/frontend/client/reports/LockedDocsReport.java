package com.logicaldoc.gui.frontend.client.reports;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.LockedDocsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of locked documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class LockedDocsReport extends AdminPanel {
	private RefreshableListGrid list;

	private SelectItem userSelector;

	public LockedDocsReport() {
		super("lockeddocs");
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		userSelector = ItemFactory.newUserSelector("user", "user", null, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(100);
		userSelector.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				refresh(Long.parseLong(userSelector.getValueAsString()));
			}
		});
		toolStrip.addFormItem(userSelector);

		ToolStripButton print = new ToolStripButton();
		print.setIcon(ItemFactory.newImgIcon("printer.png").getSrc());
		print.setTooltip(I18N.message("print"));
		print.setAutoFit(true);
		print.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});
		toolStrip.addSeparator();
		toolStrip.addButton(print);

		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addSeparator();
			ToolStripButton export = new ToolStripButton();
			export.setIcon(ItemFactory.newImgIcon("table_row_insert.png").getSrc());
			export.setTooltip(I18N.message("export"));
			export.setAutoFit(true);
			toolStrip.addButton(export);
			export.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					Util.exportCSV(list, false);
				}
			});
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();

		// Prepare a panel containing a title and the documents list
		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField id = new ListGridField("id");
		id.setHidden(true);
		id.setCanGroupBy(false);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.RIGHT);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);
		size.setCanGroupBy(false);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);
		icon.setCanGroupBy(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(false);
		version.setCanGroupBy(false);

		ListGridField fileVersion = new ListGridField("fileVersion", I18N.message("fileversion"), 55);
		fileVersion.setAlign(Alignment.CENTER);
		fileVersion.setCanFilter(false);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		lastModified.setCanGroupBy(false);

		ListGridField user = new ListGridField("username", I18N.message("lockedby"), 200);
		user.setAlign(Alignment.CENTER);
		user.setCanFilter(true);
		user.setCanGroupBy(true);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);
		customId.setCanFilter(true);

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);
		type.setCanFilter(true);

		ListGridField statusIcons = new ListGridField("statusIcons", " ");
		statusIcons.setWidth(110);
		statusIcons.setCanFilter(false);
		statusIcons.setCanSort(false);

		list = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (getFieldName(colNum).equals("filename")) {
					if ("stop".equals(record.getAttribute("immutable"))) {
						return "color: #888888; font-style: italic;";
					} else {
						return super.getCellCSSText(record, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}

			@Override
			protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {
				String fieldName = this.getFieldName(colNum);

				if (fieldName.equals("statusIcons")) {
					HLayout statusCanvas = new HLayout(3);
					statusCanvas.setHeight(22);
					statusCanvas.setWidth100();
					statusCanvas.setMembersMargin(1);
					statusCanvas.setAlign(Alignment.CENTER);

					// Put the status icon
					{
						if (record.getAttribute("status") != null) {
							Integer status = record.getAttributeAsInt("status");
							if (status != null && status.intValue() > 0) {
								ToolStripButton statusIcon = AwesomeFactory.newLockedButton(status,
										record.getAttributeAsString("username"));
								statusCanvas.addMember(statusIcon);
							}
						}
					}

					// Put the immutable icon
					{
						if (record.getAttribute("immutable") != null) {
							Integer immutable = record.getAttributeAsInt("immutable");
							if (immutable != null && immutable.intValue() == 1) {
								ToolStripButton immutableIcon = AwesomeFactory.newIconButton("hand-paper", "immutable");
								statusCanvas.addMember(immutableIcon);
							}
						}
					}
					return statusCanvas;
				} else {
					return null;
				}
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(false);
		list.setAutoFetchData(true);
		list.setFilterOnKeypress(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setShowFilterEditor(true);
		list.setFields(statusIcons, icon, filename, version, fileVersion, size, lastModified, user, customId, type);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				Long id = list.getSelectedRecord().getAttributeAsLong("id");
				DocUtil.download(id, null);
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(list.getTotalRows())));
			}
		});

		body.setMembers(toolStrip, infoPanel, list);
		refresh(null);
	}

	private void refresh(Long userId) {
		list.refresh(new LockedDocsDS(userId));
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem unlock = new MenuItem();
		unlock.setTitle(I18N.message("unlock"));
		unlock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++) {
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));
				}

				DocumentService.Instance.get().unlock(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						if (userSelector.getValue() != null)
							refresh(Long.parseLong(userSelector.getValueAsString()));
						else
							refresh(null);
					}
				});
			}
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				long id = Long.parseLong(list.getSelectedRecord().getAttribute("id"));
				DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				});
			}
		});

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				String id = list.getSelectedRecord().getAttribute("id");
				WindowUtils.openUrl(GWT.getHostPageBaseURL() + "download?docId=" + id);
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("id")));
			}
		});

		if (!(list.getSelectedRecords() != null && list.getSelectedRecords().length == 1)) {
			download.setEnabled(false);
			preview.setEnabled(false);
			openInFolder.setEnabled(false);
		}

		contextMenu.setItems(download, preview, unlock, openInFolder);
		contextMenu.showContextMenu();
	}
}