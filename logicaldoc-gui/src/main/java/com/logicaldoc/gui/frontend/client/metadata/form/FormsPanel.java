package com.logicaldoc.gui.frontend.client.metadata.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.data.FormsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
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
 * Panel showing the list of forms
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FormsPanel extends AdminPanel {

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FORM;

	final static Canvas SELECT_FORM = new HTMLPanel("&nbsp;" + I18N.message("selectform"));

	public FormsPanel() {
		super("forms");
	}

	@Override
	public void onDraw() {
		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField id = new ListGridField("id", 70);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);

		ListGridField formId = new ListGridField("formId", 150);
		formId.setHidden(true);

		ListGridField webEnabled = new ListGridField("webEnabled", I18N.message("web"), 50);

		ListGridField permaLink = new ListGridField("preview", I18N.message("preview"), 90);
		permaLink.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (record.getAttributeAsBoolean("webEnabled")) {
					return "<a href='" + webformURL(record.getAttributeAsString("formId")) + "' target='_blank'>"
							+ I18N.message("preview") + "</a>";
				} else
					return "";
			}
		});

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setCanSort(false);
		list.setFilterOnKeypress(true);
		list.setDataSource(new FormsDS());

		if (Feature.enabled(Feature.WEB_FORM))
			list.setFields(id, formId, name, webEnabled, permaLink);
		else
			list.setFields(id, formId, name);

		listing.addMember(infoPanel);
		listing.addMember(list);

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
				refresh();
			}
		});

		ToolStripButton addForm = new ToolStripButton();
		addForm.setTitle(I18N.message("addform"));
		toolStrip.addButton(addForm);
		addForm.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				FormCreate dialog = new FormCreate(FormsPanel.this);
				dialog.show();
			}
		});

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
					FormService.Instance.get().getById(Long.parseLong(record.getAttributeAsString("id")),
							new AsyncCallback<GUIForm>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIForm form) {
									showFormDetails(form);
								}
							});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showforms", Integer.toString(list.getTotalRows())));
			}
		});

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));
		final String formId = record.getAttributeAsString("formId");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							FormService.Instance.get().delete(id, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
									showFormDetails(null);
								}
							});
						}
					}
				});
			}
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				onEdit();
			}
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				WindowUtils.openUrlInNewTab(webformURL(formId));
			}
		});
		preview.setEnabled(record.getAttributeAsBoolean("webEnabled"));

		MenuItem invite = new MenuItem();
		invite.setTitle(I18N.message("invite"));
		invite.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (getSelectedForm() != null) {
					FormInvitationDialog invitation = new FormInvitationDialog(getSelectedForm().getId());
					invitation.show();
				}
			}
		});
		invite.setEnabled(record.getAttributeAsBoolean("webEnabled"));

		if (Feature.enabled(Feature.WEB_FORM))
			contextMenu.setItems(edit, preview, invite, delete);
		else
			contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}

	public void showFormDetails(GUIForm form) {
		detailsContainer.removeMember(details);

		if (form != null) {
			details = new FormDetailsPanel(this);
			detailsContainer.addMember(details);
			((FormDetailsPanel) details).setForm(form);
		} else {
			details = SELECT_FORM;
			detailsContainer.addMember(details);
		}
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param form updates the form document
	 */
	public void updateRecord(GUIForm form) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, form.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", form.getId());
			record.setAttribute("formId", form.getFormId());
			record.setAttribute("name", form.getName());
			record.setAttribute("webEnabled", form.isWebEnabled());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("name", form.getName());
		record.setAttribute("webEnabled", form.isWebEnabled());
		list.refreshRow(list.getRecordIndex(record));
	}

	private GUIForm getSelectedForm() {
		ListGridRecord record = list.getSelectedRecord();
		if (record == null)
			return null;

		GUIForm form = new GUIForm();
		form.setId(Long.parseLong(record.getAttributeAsString("id")));
		form.setName(record.getAttributeAsString("name"));
		return form;
	}

	public void refresh() {
		list.refresh(new FormsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FORM;
		detailsContainer.setMembers(details);
	}

	private void onEdit() {
		if (getSelectedForm() != null && details instanceof FormDetailsPanel) {
			if (((FormDetailsPanel) details).getForm().getId() == getSelectedForm().getId())
				((FormDetailsPanel) details).openContentEditor();
		}
	}

	public static String webformURL(String formId) {
		String url = Util.contextPath() + "webform/" + formId;
		return url;
	}
}