package com.logicaldoc.gui.frontend.client.metadata;

import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeEngine;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.logicaldoc.gui.frontend.client.services.BarcodeServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the barcode processing infos.
 * 
 * @author Marco Mescheri - Logical Objects
 * @since 6.1
 */
public class BarcodesSettingsPanel extends AdminPanel {
	private BarcodeServiceAsync service = (BarcodeServiceAsync) GWT.create(BarcodeService.class);

	private Layout engineTabPabel;

	private Layout processingQueueTabPanel;

	private Layout schemeTabPanel;

	private GUIBarcodeEngine engine;

	private ValuesManager vm = new ValuesManager();

	private ListGrid docsList;

	private DocumentsDS dataSource;

	private Tab processingQueueTab;

	private InfoPanel infoPanel;

	public BarcodesSettingsPanel() {
		super("barcodes");

		service.getInfo(new AsyncCallback<GUIBarcodeEngine>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeEngine engine) {
				BarcodesSettingsPanel.this.engine = engine;

				fillEngineTab();

				Tab schemeTab = fillSchemeTab();

				BarcodesSettingsPanel.this.processingQueueTab = fillProcessingQueueTab(100);
				BarcodesSettingsPanel.this.tabs.addTab(schemeTab);
				BarcodesSettingsPanel.this.tabs.addTab(processingQueueTab);
			}
		});
	}

	private Tab fillProcessingQueueTab(int maxValue) {
		Tab indexingQueueTab = new Tab(I18N.message("processingqueue"));
		processingQueueTabPanel = new VLayout();
		processingQueueTabPanel.setWidth100();
		processingQueueTabPanel.setHeight100();

		final IntegerItem max = ItemFactory.newValidateIntegerItem("max", "", maxValue, 1, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setWidth(40);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		display.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (max.validate()) {
					refresh((Integer) max.getValue());
				}
			}
		});

		// Prepare a panel containing a title and the documents number
		infoPanel = new InfoPanel("");

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField size = new ListGridField("size", I18N.message("size"), 70);
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		ListGridField version = new ListGridField("version", I18N.message("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setCanFilter(true);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);

		ListGridField publisher = new ListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);
		publisher.setCanFilter(true);

		ListGridField published = new ListGridField("published", I18N.message("publishedon"), 110);
		published.setAlign(Alignment.CENTER);
		published.setType(ListGridFieldType.DATE);
		published.setCellFormatter(new DateCellFormatter(false));
		published.setCanFilter(false);

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);
		creator.setCanFilter(true);

		ListGridField created = new ListGridField("created", I18N.message("createdon"), 110);
		created.setAlign(Alignment.CENTER);
		created.setType(ListGridFieldType.DATE);
		created.setCellFormatter(new DateCellFormatter(false));
		created.setCanFilter(false);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
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

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setCanFilter(true);

		ListGridField lockUserId = new ListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);
		lockUserId.setCanFilter(false);

		docsList = new ListGrid() {
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
		docsList.setEmptyMessage(I18N.message("notitemstoshow"));

		docsList.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showQueueMenu();
				event.cancel();
			}
		});

		docsList.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showndocuments", Integer.toString(docsList.getTotalRows())));
			}
		});

		docsList.setShowRecordComponents(true);
		docsList.setShowRecordComponentsByCell(true);
		docsList.setCanFreezeFields(true);
		docsList.setAutoFetchData(true);
		docsList.setSelectionType(SelectionStyle.MULTIPLE);
		docsList.setShowFilterEditor(true);
		docsList.setFilterOnKeypress(true);
		dataSource = new DocumentsDS(null, null, maxValue, 1, null, 0);
		docsList.setDataSource(dataSource);
		docsList.setFields(locked, immutable, icon, filename, size, lastModified, version, publisher, published,
				creator, created, customId);

		processingQueueTabPanel.setMembers(toolStrip, infoPanel, docsList);
		indexingQueueTab.setPane(processingQueueTabPanel);
		return indexingQueueTab;
	}

	private void fillEngineTab() {
		engineTabPabel = new VLayout();
		engineTabPabel.setWidth100();
		engineTabPabel.setHeight100();

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);
		form.setWrapItemTitles(false);
		form.setColWidths(1, "*");
		form.setValuesManager(vm);

		// Include Patters
		TextItem includePatters = ItemFactory.newTextItem("includePatters", "includepatters",
				this.engine.getIncludePatters());
		includePatters.setHint(I18N.message("separatedcomma"));
		includePatters.setHintStyle("hint");

		// Exclude Patters
		TextItem excludePatters = ItemFactory.newTextItem("excludePatters", "excludepatters",
				this.engine.getExcludePatters());
		excludePatters.setHint(I18N.message("separatedcomma"));
		excludePatters.setHintStyle("hint");

		// The optional batch
		IntegerItem batch = ItemFactory.newIntegerItem("batch", "batch", this.engine.getBatch());
		batch.setHintStyle("hint");

		// The image threshold
		IntegerItem resolutionThreshold = ItemFactory.newIntegerItem("resolutionthreshold",
				I18N.message("resolutionthreshold"), this.engine.getImageThreshold());
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setHint("pixels");

		HLayout buttons = new HLayout();

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				@SuppressWarnings("unchecked")
				final Map<String, Object> values = vm.getValues();

				if (vm.validate()) {
					BarcodesSettingsPanel.this.engine.setIncludePatters((String) values.get("includePatters"));
					BarcodesSettingsPanel.this.engine.setExcludePatters((String) values.get("excludePatters"));
					String btch = vm.getValueAsString("batch");
					if (btch == null || "".equals(btch.trim()))
						BarcodesSettingsPanel.this.engine.setBatch(0);
					else
						BarcodesSettingsPanel.this.engine.setBatch(new Integer(btch));

					String threshold = vm.getValueAsString("resolutionthreshold");
					if (btch == null || "".equals(btch.trim()))
						BarcodesSettingsPanel.this.engine.setImageThreshold(0);
					else
						BarcodesSettingsPanel.this.engine.setImageThreshold(new Integer(threshold));

					service.save(BarcodesSettingsPanel.this.engine, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							Log.info(I18N.message("settingssaved"), null);
						}
					});
				}
			}
		});

		IButton rescheduleAll = new IButton();
		rescheduleAll.setAutoFit(true);
		rescheduleAll.setTitle(I18N.message("rescheduleallprocessing"));
		rescheduleAll.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				service.rescheduleAll(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						Log.info(I18N.message("docsrescheduledprocessing"), null);
					}
				});
			}
		});

		if (Session.get().isDefaultTenant())
			form.setItems(includePatters, excludePatters, batch, resolutionThreshold);
		else
			form.setItems(includePatters, excludePatters);
		buttons.setMembers(save, rescheduleAll);
		buttons.setMembersMargin(5);
		engineTabPabel.setMembers(form, buttons);
		engineTabPabel.setMembersMargin(15);
		engineTabPabel.setMargin(5);
		tab.setPane(engineTabPabel);
	}

	private void refresh(Integer max) {
		fillProcessingQueueTab(max);
		tabs.setTabPane(2, processingQueueTabPanel);
	}

	private void showQueueMenu() {
		final ListGridRecord[] selection = docsList.getSelection();

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

				service.markUnprocessable(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (ListGridRecord record : selection) {
							docsList.removeData(record);
						}
					}
				});
			}
		});

		contextMenu.setItems(markUnprocessable);
		contextMenu.showContextMenu();
	}

	private Tab fillSchemeTab() {
		Tab schemeTab = new Tab(I18N.message("patterns"));
		schemeTabPanel = new VLayout();
		schemeTabPanel.setWidth100();
		schemeTabPanel.setHeight100();
		schemeTab.setPane(new BarcodePatternsPanel());
		return schemeTab;
	}

}