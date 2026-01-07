package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.IntegerListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of embedding schemes
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemesPanel extends VLayout {

	private static final String EMBEDDINGS = "embeddings";

	private static final String MODEL = "model";

	private static final String ENABLED = "eenabled";

	private static final String LABEL = "label";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_EMBEDDINGSCHEME;

	static final Canvas SELECT_EMBEDDINGSCHEME = new HTMLPanel("&nbsp;" + I18N.message("selectanembeddingscheme"));

	public EmbeddingSchemesPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_EMBEDDINGSCHEME;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField enabled = new EnabledListGridField();

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanFilter(true);
		name.setCanSort(true);
		name.setMinWidth(110);
		name.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL));
		label.setMinWidth(110);
		label.setCanFilter(true);
		label.setCanSort(true);
		label.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanFilter(true);
		type.setCanSort(true);
		type.setAutoFitWidth(true);
		label.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField model = new ListGridField(MODEL, I18N.message(MODEL));
		model.setCanFilter(true);
		model.setCanSort(true);
		model.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField embeddings = new IntegerListGridField(EMBEDDINGS, I18N.message(EMBEDDINGS));
		embeddings.setCanFilter(true);
		embeddings.setCanSort(true);
		embeddings.setAlign(Alignment.LEFT);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, name, label, type, model, embeddings);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new EmbeddingSchemesDS(null));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			list.refresh(new EmbeddingSchemesDS(null));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_EMBEDDINGSCHEME;
			detailsContainer.setMembers(details);
		});
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addembeddingscheme"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddEmbeddingScheme());

		toolStrip.addSeparator();

		ToolStripButton settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		settings.addClickHandler(event -> new EmbeddingSettings().show());
		if (Session.get().isDefaultTenant())
			toolStrip.addButton(settings);

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> showSelectedScheme());

		list.addDataArrivedHandler(event -> infoPanel
				.setMessage(I18N.message("showembeddingsechemes", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	protected void showSelectedScheme() {
		Record rec = list.getSelectedRecord();
		if (rec != null)
			AIService.Instance.get().getEmbeddingScheme(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
				@Override
				protected void handleSuccess(GUIEmbeddingScheme result) {
					showEmbeddingSchemeDetails(result);
				}
			});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong("id"));

		Long selectedEmbeddingId = selection[0].getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				AIService.Instance.get().deleteEmbeddingSchemes(ids, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showEmbeddingSchemeDetails(null);
					}
				});
			}
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> AIService.Instance.get().enableEmbeddingScheme(selectedEmbeddingId, true,
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
					}
				}));
		enable.setEnabled(!list.getSelectedRecord().getAttributeAsBoolean(ENABLED));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> AIService.Instance.get().enableEmbeddingScheme(selectedEmbeddingId, false,
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
					}
				}));
		disable.setEnabled(list.getSelectedRecord().getAttributeAsBoolean(ENABLED));

		MenuItem reset = new MenuItem();
		reset.setTitle(I18N.message("removeembeddings"));
		reset.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmremoveembeddings"), confirm -> {
					if (Boolean.TRUE.equals(confirm)) {
						LD.contactingServer();
						AIService.Instance.get().removeEmbeddings(ids.get(0), null, new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(Void result) {
								LD.clearPrompt();
								GuiLog.info(I18N.message("embeddingsremoved"), null);
								list.getSelectedRecord().setAttribute(EMBEDDINGS, 0);
								list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
								showSelectedScheme();
							}
						});
					}
				}));

		contextMenu.setItems(enable, disable, new MenuItemSeparator(), reset, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	protected void showEmbeddingSchemeDetails(GUIEmbeddingScheme embeddingScheme) {
		detailsContainer.removeMember(details);
		if (embeddingScheme != null)
			details = new EmbeddingSchemeDetailsPanel(this);
		else
			details = SELECT_EMBEDDINGSCHEME;
		detailsContainer.addMember(details);
		((EmbeddingSchemeDetailsPanel) details).setEmbeddingScheme(embeddingScheme);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param embeddingScheme the embeddingScheme to take data from
	 */
	public void updateRecord(GUIEmbeddingScheme embeddingScheme) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, embeddingScheme.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", embeddingScheme.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", embeddingScheme.getName());
		rec.setAttribute(LABEL,
				embeddingScheme.getLabel() != null ? embeddingScheme.getLabel() : embeddingScheme.getName());

		rec.setAttribute("type", embeddingScheme.getType());
		rec.setAttribute(ENABLED, embeddingScheme.isEnabled());
		rec.setAttribute(MODEL, embeddingScheme.getModel());
		rec.setAttribute(EMBEDDINGS, embeddingScheme.getEmbeddings());

		list.refreshRow(list.getRecordIndex(rec));
	}

	protected void onAddEmbeddingScheme() {
		list.deselectAllRecords();
		showEmbeddingSchemeDetails(new GUIEmbeddingScheme());
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
