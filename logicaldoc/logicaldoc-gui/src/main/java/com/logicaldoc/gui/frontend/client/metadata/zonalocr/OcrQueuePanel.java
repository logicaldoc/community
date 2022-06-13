package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the queue of documents to be processed by the Zonal OCR task.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class OcrQueuePanel extends VLayout {

	private RefreshableListGrid list;

	private int maxRecords = 10;

	private InfoPanel infoPanel = null;

	public OcrQueuePanel(int maxRecords) {
		this.maxRecords = maxRecords;
		setWidth100();
		setHeight100();
		setMembersMargin(5);
	}

	public int getMaxRecords() {
		return maxRecords;
	}

	public void setMaxRecords(int maxRecords) {
		this.maxRecords = maxRecords;
	}

	@Override
	public void onDraw() {
		final SpinnerItem max = ItemFactory.newSpinnerItem("max", "", maxRecords);
		max.setWidth(80);
		max.setMin(5);
		max.setStep(10);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (max.validate()) {
					maxRecords = (Integer) max.getValue();
					list.refresh(new DocumentsDS(null, null, maxRecords, 1, null, false, true, null));
				}
			}
		});

		ToolStripButton reschedule = new ToolStripButton();
		reschedule.setTitle(I18N.message("rescheduleallprocessing"));
		reschedule.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("rescheduleallprocessing"), I18N.message("rescheduleallprocessingask"),
						new BooleanCallback() {

							@Override
							public void execute(Boolean value) {
								if (value)
									ZonalOCRService.Instance.get().rescheduleAll(new AsyncCallback<Void>() {

										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void ret) {
											GuiLog.info(I18N.message("docsrescheduledprocessing"), null);
											maxRecords = (Integer) max.getValue();
											list.refresh(new DocumentsDS(null, null, maxRecords, 1, null, false, true,
													null));
										}
									});
							}
						});
			}
		});

		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		toolStrip.addSeparator();
		toolStrip.addButton(reschedule);

		// Prepare a panel containing a title and the documents number
		infoPanel = new InfoPanel("");

		ListGridField id = new ColoredListGridField("id");
		id.setHidden(true);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);
		size.setCanFilter(false);

		ListGridField version = new VersionListGridField();
		version.setCanFilter(true);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");
		lastModified.setCanFilter(false);

		ListGridField publisher = new ColoredListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);
		publisher.setCanFilter(true);

		ListGridField published = new DateListGridField("published", "publishedon");

		ListGridField creator = new ColoredListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);
		creator.setCanFilter(true);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		customId.setCanFilter(false);

		ListGridField immutable = new ListGridField("immutable", " ", 24);
		immutable.setType(ListGridFieldType.IMAGE);
		immutable.setCanSort(false);
		immutable.setAlign(Alignment.CENTER);
		immutable.setShowDefaultContextMenu(false);
		immutable.setImageURLPrefix(Util.imagePrefix());
		immutable.setImageURLSuffix(".png");
		immutable.setCanFilter(false);

		ListGridField locked = new ListGridField("locked", " ", 24);
		locked.setType(ListGridFieldType.IMAGE);
		locked.setCanSort(false);
		locked.setAlign(Alignment.CENTER);
		locked.setShowDefaultContextMenu(false);
		locked.setImageURLPrefix(Util.imagePrefix());
		locked.setImageURLSuffix(".png");
		locked.setCanFilter(false);

		ListGridField filename = new FileNameListGridField();
		filename.setWidth(200);
		filename.setCanFilter(true);

		ListGridField lockUserId = new ColoredListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);
		lockUserId.setCanFilter(false);

		list = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (record == null)
					return "";
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

		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setShowFilterEditor(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new DocumentsDS(null, null, maxRecords, 1, null, false, true, null));
		list.setFields(locked, immutable, filename, size, lastModified, version, publisher, published, creator, created,
				customId);

		setMembers(toolStrip, infoPanel, list);
	}

	private void showContextMenu() {
		final ListGridRecord[] selection = list.getSelectedRecords();

		Menu contextMenu = new Menu();
		MenuItem markUnprocessable = new MenuItem();
		markUnprocessable.setTitle(I18N.message("markunprocessable"));
		markUnprocessable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null)
					return;
				final long[] ids = new long[selection.length];
				for (int j = 0; j < selection.length; j++) {
					ids[j] = Long.parseLong(selection[j].getAttribute("id"));
				}

				ZonalOCRService.Instance.get().markUnprocessable(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : selection) {
							list.removeData(record);
						}
					}
				});
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				if (record == null)
					return;

				DocumentsPanel.get().openInFolder(record.getAttributeAsLong("id"));
			}
		});

		contextMenu.setItems(markUnprocessable, openInFolder);
		contextMenu.showContextMenu();
	}
}