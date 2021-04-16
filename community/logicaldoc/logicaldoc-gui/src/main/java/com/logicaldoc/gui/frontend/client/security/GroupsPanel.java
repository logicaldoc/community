package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
 * This panel shows the list of users and a detail area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupsPanel extends AdminPanel {

	private RefreshableListGrid list;

	final static Canvas SELECT_GROUP = new HTMLPanel("&nbsp;" + I18N.message("selectgroup"));

	private Canvas details = SELECT_GROUP;

	private Layout detailsContainer = null;

	public GroupsPanel() {
		super("groups");
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.refresh(new GroupsDS());
				detailsContainer.removeMembers(detailsContainer.getMembers());
				details = SELECT_GROUP;
				detailsContainer.setMembers(details);
			}
		});
		toolStrip.addSeparator();

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addgroup"));
		toolStrip.addButton(add);
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				showGroupDetails(new GUIGroup());
			}
		});
		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		toolStrip.addSeparator();

		ToolStripButton print = new ToolStripButton();
		print.setTitle(I18N.message("print"));
		toolStrip.addButton(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});
		toolStrip.addFill();

		detailsContainer = new VLayout();

		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel as placeholder
		final Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);
		name.setCanFilter(true);

		ListGridField source = new ListGridField("source", I18N.message("source"), 100);
		source.setCanFilter(true);
		source.setHidden(true);

		ListGridField description = new ListGridField("description", I18N.message("description"), 200);
		description.setCanFilter(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new GroupsDS());
		list.setFields(id, name, description, source);

		listing.addMember(infoPanel);
		listing.addMember(list);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record record = list.getSelectedRecord();
				if (record != null)
					SecurityService.Instance.get().getGroup(Long.parseLong(record.getAttributeAsString("id")),
							new AsyncCallback<GUIGroup>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIGroup group) {
									showGroupDetails(group);
								}
							});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showgroups", Integer.toString(list.getTotalRows())));
			}
		});

		details = SELECT_GROUP;
		detailsContainer.setMembers(details);

		body.addMembers(toolStrip, listing, detailsContainer);
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param group the group to update
	 */
	public void updateRecord(GUIGroup group) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, group.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", group.getId());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("description", group.getDescription());
		record.setAttribute("name", group.getName());
		list.refreshRow(list.getRecordIndex(record));
	}

	public void showGroupDetails(GUIGroup group) {
		if (!(details instanceof GroupDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new GroupDetailsPanel(GroupsPanel.this);
			detailsContainer.addMember(details);
		}
		((GroupDetailsPanel) details).setGroup(group);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SecurityService.Instance.get().deleteGroup(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									details = SELECT_GROUP;
									detailsContainer.setMembers(details);
								}
							});
						}
					}
				});
			}
		});

		if (Constants.GROUP_ADMIN.equals(record.getAttributeAsString("name"))
				|| Constants.GROUP_PUBLISHER.equals(record.getAttributeAsString("name"))
				|| Constants.GROUP_GUEST.equals(record.getAttributeAsString("name"))) {
			delete.setEnabled(false);
		}

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}