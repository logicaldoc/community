package com.logicaldoc.gui.frontend.client.ai.filler;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.RunningListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSettings;
import com.logicaldoc.gui.frontend.client.ai.model.AIStats;
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
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of fillers
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class FillersPanel extends VLayout {

	private static final String QUESTION = "question";

	private static final String TRAINED = "trained";

	private static final String ID = "id";

	private static final String TRAINING = "training";

	static final Canvas SELECT_FILLER = new HTMLPanel("&nbsp;" + I18N.message("selectafiller"));

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_FILLER;

	private Timer timer;

	public FillersPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_FILLER;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanFilter(true);
		name.setCanSort(true);
		name.setMinWidth(110);
		name.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL), 200);
		label.setCanFilter(true);
		label.setCanSort(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		ListGridField fillerType = new ListGridField("type", I18N.message("type"));
		fillerType.setAutoFit(AutoFitWidthApproach.BOTH);
		fillerType.setCellFormatter((value, rcrd, rowNum, colNum) -> I18N.message("aifillertype." + value));

		ListGridField training = new RunningListGridField(TRAINING);
		training.setTitle(I18N.message(TRAINING));
		training.setAutoFitWidth(true);
		training.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		DateListGridField trained = new DateListGridField(TRAINED, I18N.message("lasttrained"));

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, name, label, fillerType, training, trained, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new FillersDS(null));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(click -> refresh());
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addfiller"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddFiller());

		toolStrip.addSeparator();

		ToolStripButton settings = new ToolStripButton();
		settings.setTitle(I18N.message("settings"));
		toolStrip.addButton(settings);
		settings.addClickHandler(event -> new EmbeddingSettings().show());

		ToolStripButton stats = new ToolStripButton();
		stats.setTitle(I18N.message("stats"));
		toolStrip.addButton(stats);
		stats.addClickHandler(event -> new AIStats().show());

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				FillerService.Instance.get().get(rec.getAttributeAsLong(ID), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFiller filler) {
						showFillerDetails(filler);
					}
				});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showfillers", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	private void refresh() {
		list.refresh(new FillersDS(null));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FILLER;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong(ID));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message(QUESTION), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				FillerService.Instance.get().delete(ids, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showFillerDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);

		contextMenu.showContextMenu();
	}

	protected void showFillerDetails(GUIFiller filler) {
		detailsContainer.removeMember(details);
		if (filler != null)
			details = new FillerDetailsPanel(this);
		else
			details = SELECT_FILLER;
		detailsContainer.addMember(details);
		((FillerDetailsPanel) details).setFiller(filler);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param filler the filler to take data from
	 */
	public void updateRecord(GUIFiller filler) {
		Record rec = list.find(new AdvancedCriteria(ID, OperatorId.EQUALS, filler.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute(ID, filler.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("type", filler.getType());
		rec.setAttribute("name", filler.getName());
		rec.setAttribute(LABEL, filler.getLabel() != null && !filler.getLabel().trim().isEmpty() ? filler.getLabel()
				: filler.getName());
		rec.setAttribute(DESCRIPTION, filler.getDescription());
		list.refreshRow(list.getRecordIndex(rec));
	}

	protected void onAddFiller() {
		list.deselectAllRecords();
		showFillerDetails(new GUIFiller());
	}

	@Override
	public void destroy() {
		super.destroy();
		this.timer.cancel();
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
