package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.data.AttributeSetsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of attribute sets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetsPanel extends VLayout {

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_SET;

	private InfoPanel infoPanel;

	private ToolStrip toolStrip;

	final static Canvas SELECT_SET = new HTMLPanel("&nbsp;" + I18N.message("selecttattributeset"));

	public AttributeSetsPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_SET;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 200);
		name.setCanFilter(true);
		name.setCanSort(true);

		ListGridField description = new ListGridField("description", I18N.message("description"), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 100);
		documents.setCanSort(false);
		documents.setCanFilter(false);

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
		list.setFields(name, description, documents);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new AttributeSetsDS(false, GUITemplate.TYPE_DEFAULT));
		list.setShowFilterEditor(true);

		listing.addMember(infoPanel);
		listing.addMember(list);

		toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.refresh(new AttributeSetsDS(false, GUITemplate.TYPE_DEFAULT));
				detailsContainer.removeMembers(detailsContainer.getMembers());
				details = SELECT_SET;
				detailsContainer.setMembers(details);
			}
		});
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addattributeset"));
		toolStrip.addButton(add);
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onAddAttributeSet();
			}
		});

		toolStrip.addFill();

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				ListGridRecord rec = list.getSelectedRecord();
				if (!"true".equals(rec.getAttributeAsString("readonly"))) {
					showContextMenu();
				}
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record rec = list.getSelectedRecord();
				if (rec != null)
					AttributeSetService.Instance.get().getAttributeSet(rec.getAttributeAsLong("id"),
							new AsyncCallback<GUIAttributeSet>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAttributeSet attSet) {
									showSetDetails(attSet);
								}
							});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showattributesets", Integer.toString(list.getTotalRows())));
			}
		});

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
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"),(Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
							AttributeSetService.Instance.get().delete(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									showSetDetails(null);
								}
							});
						}
				});
			}
		});

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
		rec.setAttribute("description", set.getDescription());
		list.refreshRow(list.getRecordIndex(rec));

	}

	protected void onAddAttributeSet() {
		list.deselectAllRecords();
		showSetDetails(new GUIAttributeSet());
	}
}