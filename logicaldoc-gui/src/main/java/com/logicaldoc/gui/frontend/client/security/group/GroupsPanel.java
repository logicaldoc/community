package com.logicaldoc.gui.frontend.client.security.group;

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
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of users and a detail area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupsPanel extends AdminPanel {

	private static final String DESCRIPTION = "description";

	private RefreshableListGrid list;

	static final Canvas SELECT_GROUP = new HTMLPanel("&nbsp;" + I18N.message("selectgroup"));

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
		refresh.addClickHandler(event -> {
			list.refresh(new GroupsDS());
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_GROUP;
			detailsContainer.setMembers(details);
		});
		toolStrip.addSeparator();

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addgroup"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> {
			list.deselectAllRecords();
			showGroupDetails(new GUIGroup());
		});
		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(event -> GridUtil.exportCSV(list, true));
		if (!Feature.enabled(Feature.EXPORT_CSV)) {
			export.setDisabled(true);
			export.setTooltip(I18N.message("featuredisabled"));
		}
		toolStrip.addSeparator();

		ToolStripButton print = new ToolStripButton();
		print.setTitle(I18N.message("print"));
		toolStrip.addButton(print);
		print.addClickHandler(event -> GridUtil.print(list));
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

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 200);
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

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				SecurityService.Instance.get().getGroup(Long.parseLong(rec.getAttributeAsString("id")),
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
		});

		list.addDataArrivedHandler((DataArrivedEvent event) -> infoPanel
				.setMessage(I18N.message("showgroups", Integer.toString(list.getTotalRows()))));

		details = SELECT_GROUP;
		detailsContainer.setMembers(details);

		body.addMembers(toolStrip, listing, detailsContainer);
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param group the group to update
	 */
	public void updateRecord(GUIGroup group) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, group.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", group.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute(DESCRIPTION, group.getDescription());
		rec.setAttribute("name", group.getName());
		list.refreshRow(list.getRecordIndex(rec));
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

		ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
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
				}));

		if (Constants.GROUP_ADMIN.equals(rec.getAttributeAsString("name"))
				|| Constants.GROUP_PUBLISHER.equals(rec.getAttributeAsString("name"))
				|| Constants.GROUP_GUEST.equals(rec.getAttributeAsString("name"))) {
			delete.setEnabled(false);
		}

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}