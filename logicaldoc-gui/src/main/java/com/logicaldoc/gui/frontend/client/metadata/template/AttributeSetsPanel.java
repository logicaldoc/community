package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.data.AttributeSetsDS;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
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
 * Panel showing the list of attribute sets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetsPanel extends VLayout {

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_SET;

	static final Canvas SELECT_SET = new HTMLPanel("&nbsp;" + I18N.message("selecttattributeset"));

	public AttributeSetsPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_SET;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"), 200);
		name.setCanFilter(true);
		name.setCanSort(true);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL), 200);
		label.setCanFilter(true);
		label.setCanSort(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		ListGridField typeSet = new ListGridField("type", I18N.message("type"), 100);
		typeSet.setHidden(true);

		ListGridField signRequired = new ListGridField("signRequired", I18N.message("signrequired"), 100);
		signRequired.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, name, label, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new AttributeSetsDS(false, GUITemplate.TYPE_DEFAULT));
		list.setShowFilterEditor(true);

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			list.refresh(new AttributeSetsDS(false, GUITemplate.TYPE_DEFAULT));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_SET;
			detailsContainer.setMembers(details);
		});
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addattributeset"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddAttributeSet());

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			if (!Boolean.parseBoolean(rec.getAttributeAsString("readonly"))) {
				showContextMenu();
			}
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				AttributeSetService.Instance.get().getAttributeSet(rec.getAttributeAsLong("id"),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(GUIAttributeSet attSet) {
								showSetDetails(attSet);
							}
						});
		});

		list.addDataArrivedHandler(event -> infoPanel
				.setMessage(I18N.message("showattributesets", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = rec.getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				AttributeSetService.Instance.get().delete(id, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showSetDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	protected void showSetDetails(GUIAttributeSet attSet) {
		if (!(details instanceof AttributeSetDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new AttributeSetDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((AttributeSetDetailsPanel) details).setAttributeSet(attSet);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param set the attribute set to update
	 */
	public void updateRecord(GUIAttributeSet set) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, set.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", set.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("readonly", "" + set.isReadonly());
		rec.setAttribute("name", set.getName());
		rec.setAttribute(LABEL, set.getLabel() != null ? set.getLabel() : set.getName());
		rec.setAttribute(DESCRIPTION, set.getDescription());
		list.refreshRow(list.getRecordIndex(rec));

	}

	protected void onAddAttributeSet() {
		list.deselectAllRecords();
		showSetDetails(new GUIAttributeSet());
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