package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.data.TemplatesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
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
 * Panel showing the list of templates
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TemplatesPanel extends VLayout {
	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_TEMPLATE;

	static final Canvas SELECT_TEMPLATE = new HTMLPanel("&nbsp;" + I18N.message("selecttemplate"));

	public TemplatesPanel() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		Canvas[] members = getMembers();
		for (Canvas canvas : members) {
			removeMember(canvas);
		}

		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_TEMPLATE;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 200);
		name.setCanFilter(true);
		name.setCanSort(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		ListGridField typeTemplate = new ListGridField("type", I18N.message("type"), 100);
		typeTemplate.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(name, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new TemplatesDS(false, null, GUITemplate.TYPE_DEFAULT));
		list.setShowFilterEditor(true);

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtemplate"));
		add.addClickHandler(event -> onAddingTemplate());

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			list.refresh(new TemplatesDS(false, null, GUITemplate.TYPE_DEFAULT));
			detailsContainer.removeMembers(detailsContainer.getMembers());
			details = SELECT_TEMPLATE;
			detailsContainer.setMembers(details);
		});

		toolStrip.addButton(refresh);
		toolStrip.addButton(add);
		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			ListGridRecord rec = list.getSelectedRecord();
			if (!"true".equals(rec.getAttributeAsString("readonly"))) {
				showContextMenu();
			}
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				TemplateService.Instance.get().getTemplate(Long.parseLong(rec.getAttributeAsString("id")),
						new AsyncCallback<GUITemplate>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUITemplate template) {
								showTemplateDetails(template);
							}
						});
		});

		list.addDataArrivedHandler((DataArrivedEvent event) -> infoPanel
				.setMessage(I18N.message("showtemplates", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				TemplateService.Instance.get().delete(id, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showTemplateDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	protected void showTemplateDetails(GUITemplate template) {
		if (!(details instanceof TemplateDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new TemplateDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((TemplateDetailsPanel) details).setTemplate(template);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param template the template to update
	 */
	public void updateRecord(GUITemplate template) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, template.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", template.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("readonly", "" + template.isReadonly());
		rec.setAttribute("name", template.getName());
		rec.setAttribute(DESCRIPTION, template.getDescription());
		list.refreshRow(list.getRecordIndex(rec));
	}

	protected void onAddingTemplate() {
		list.deselectAllRecords();
		GUITemplate newTemplate = new GUITemplate();
		newTemplate.setPermissions(new String[] { "read", "write" });
		showTemplateDetails(newTemplate);
	}
}