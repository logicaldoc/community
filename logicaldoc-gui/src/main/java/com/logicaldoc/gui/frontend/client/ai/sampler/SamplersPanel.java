package com.logicaldoc.gui.frontend.client.ai.sampler;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
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
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of samplers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplersPanel extends VLayout {

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_SAMPLER;

	static final Canvas SELECT_SAMPLER = new HTMLPanel("&nbsp;" + I18N.message("selectasampler"));

	public SamplersPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_SAMPLER;

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

		ListGridField samplerTypeField = new ListGridField("type", I18N.message("type"));
		samplerTypeField.setCellFormatter((value, rcrd, rowNum, colNum) -> I18N.message("aisamplertype." + value));
		samplerTypeField.setAutoFit(AutoFitWidthApproach.BOTH);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, name, label, samplerTypeField, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new SamplersDS(null));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			list.refresh(new SamplersDS(null));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_SAMPLER;
			detailsContainer.setMembers(details);
		});
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addsampler"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddSampler());

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				AIService.Instance.get().getSampler(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUISampler sampler) {
						showSamplerDetails(sampler);
					}
				});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showsamplers", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				AIService.Instance.get().deleteSamplers(ids, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showSamplerDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	protected void showSamplerDetails(GUISampler sampler) {
		detailsContainer.removeMember(details);
		if (sampler != null)
			details = new SamplerDetailsPanel(this);
		else
			details = SELECT_SAMPLER;
		detailsContainer.addMember(details);
		((SamplerDetailsPanel) details).setSampler(sampler);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param sampler the sampler to take data from
	 */
	public void updateRecord(GUISampler sampler) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, sampler.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", sampler.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", sampler.getName());
		rec.setAttribute(LABEL, sampler.getLabel() != null ? sampler.getLabel() : sampler.getName());
		rec.setAttribute(DESCRIPTION, sampler.getDescription());
		list.refreshRow(list.getRecordIndex(rec));

	}

	protected void onAddSampler() {
		list.deselectAllRecords();
		showSamplerDetails(new GUISampler());
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