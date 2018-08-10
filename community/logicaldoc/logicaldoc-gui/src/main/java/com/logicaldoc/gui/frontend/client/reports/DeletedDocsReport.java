package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DeletedDocsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderSelector;
import com.logicaldoc.gui.frontend.client.folder.RestoreDialog;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of deleted documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class DeletedDocsReport extends AdminPanel implements FolderChangeListener {

	private RefreshableListGrid list;

	private SelectItem userSelector;

	private FolderSelector folderSelector;

	public DeletedDocsReport() {
		super("deleteddocs");
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		userSelector = ItemFactory.newUserSelector("user", "deletedby", null, false);
		userSelector.setWrapTitle(false);
		userSelector.setWidth(100);
		userSelector.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});
		toolStrip.addFormItem(userSelector);

		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

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

		ListGridField immutable = new ListGridField("immutable", " ", 24);
		immutable.setType(ListGridFieldType.IMAGE);
		immutable.setCanSort(false);
		immutable.setAlign(Alignment.CENTER);
		immutable.setShowDefaultContextMenu(false);
		immutable.setImageURLPrefix(Util.imagePrefix());
		immutable.setImageURLSuffix(".png");
		immutable.setCanFilter(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);
		version.setCanGroupBy(false);

		ListGridField fileVersion = new ListGridField("fileVersion", I18N.message("fileversion"), 55);
		fileVersion.setAlign(Alignment.CENTER);
		fileVersion.setCanFilter(true);
		fileVersion.setCanGroupBy(false);
		fileVersion.setHidden(true);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		lastModified.setCanGroupBy(false);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setHidden(true);
		customId.setCanGroupBy(false);

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setCanFilter(true);

		ListGridField type = new ListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		type.setHidden(true);
		type.setCanGroupBy(false);

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
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(false);
		list.setAutoFetchData(true);
		list.setFilterOnKeypress(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);

		list.setFields(immutable, icon, filename, version, fileVersion, size, lastModified, customId, type);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(list.getTotalRows())));
			}
		});

		body.setMembers(toolStrip, infoPanel, list);
		
		refresh();
	}

	private void refresh() {
		Long folderId = folderSelector.getFolderId();
		Long userId = null;
		if (userSelector.getValueAsString() != null && !"".equals(userSelector.getValueAsString()))
			userId = Long.parseLong(userSelector.getValueAsString());
		list.refresh(new DeletedDocsDS(userId, folderId));
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem restore = new MenuItem();
		restore.setTitle(I18N.message("restore"));
		restore.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final Long[] ids = new Long[selection.length];
				for (int i = 0; i < selection.length; i++)
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));

				final RestoreDialog dialog = new RestoreDialog(ids, null, new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						refresh();
					}
				});
				dialog.show();
			}
		});

		contextMenu.setItems(restore);
		contextMenu.showContextMenu();
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}