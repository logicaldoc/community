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
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
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

	private static final String PREVIEW = "preview";

	private static final String WEB_ENABLED = "webEnabled";

	private static final String FORM_ID = "formId";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FORM;

	static final Canvas SELECT_FORM = new HTMLPanel("&nbsp;" + I18N.message("selectform"));

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

		ListGridField formId = new ListGridField(FORM_ID, 150);
		formId.setHidden(true);

		ListGridField webEnabled = new ListGridField(WEB_ENABLED, I18N.message("web"), 50);

		ListGridField permaLink = new ListGridField(PREVIEW, I18N.message(PREVIEW), 90);
		permaLink.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (Boolean.TRUE.equals(rec.getAttributeAsBoolean(WEB_ENABLED))) {
				return "<a href='" + webformURL(rec.getAttributeAsString(FORM_ID)) + "' target='_blank'>"
						+ I18N.message(PREVIEW) + "</a>";
			} else
				return "";
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
		list.setShowFilterEditor(true);

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
		refresh.addClickHandler(event -> refresh());

		ToolStripButton addForm = new ToolStripButton();
		addForm.setTitle(I18N.message("addform"));
		toolStrip.addButton(addForm);
		addForm.addClickHandler(event -> {
			list.deselectAllRecords();
			new FormCreate(FormsPanel.this).show();
		});

		list.addCellContextClickHandler(event -> {
			if (!PREVIEW.equals(list.getField(event.getColNum()).getName())) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				FormService.Instance.get().getById(Long.parseLong(rec.getAttributeAsString("id")),
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
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showforms", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = Long.parseLong(rec.getAttributeAsString("id"));
		final String formId = rec.getAttributeAsString(FORM_ID);

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> {
			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
				if (Boolean.TRUE.equals(value)) {
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
			});
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler((MenuItemClickEvent event) -> onEdit());

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message(PREVIEW));
		preview.addClickHandler((MenuItemClickEvent event) -> WindowUtils.openUrlInNewTab(webformURL(formId)));
		preview.setEnabled(rec.getAttributeAsBoolean(WEB_ENABLED));

		MenuItem invite = new MenuItem();
		invite.setTitle(I18N.message("invite"));
		invite.addClickHandler((MenuItemClickEvent event) -> {
			GUIForm selectedForm = getSelectedForm();
			if (selectedForm != null) {
				new WebFormInvitationDialog(selectedForm.getId()).show();
			}
		});
		invite.setEnabled(rec.getAttributeAsBoolean(WEB_ENABLED));

		MenuItem getPrefilledLink = new MenuItem();
		getPrefilledLink.setTitle(I18N.message("getprefilledlink"));
		getPrefilledLink.addClickHandler((MenuItemClickEvent event) -> {
			GUIForm selectedForm = getSelectedForm();
			if (selectedForm != null) {
				new WebFormPrefilledLink(selectedForm.getId()).show();
			}
		});
		getPrefilledLink.setEnabled(rec.getAttributeAsBoolean(WEB_ENABLED));

		if (Feature.enabled(Feature.WEB_FORM))
			contextMenu.setItems(edit, preview, invite, getPrefilledLink, delete);
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
	 * Updates the selected rec with new data
	 * 
	 * @param form updates the form document
	 */
	public void updateRecord(GUIForm form) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, form.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", form.getId());
			rec.setAttribute(FORM_ID, form.getFormId());
			rec.setAttribute("name", form.getName());
			rec.setAttribute(WEB_ENABLED, form.isWebEnabled());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", form.getName());
		rec.setAttribute(WEB_ENABLED, form.isWebEnabled());
		list.refreshRow(list.getRecordIndex(rec));
	}

	private GUIForm getSelectedForm() {
		ListGridRecord rec = list.getSelectedRecord();
		if (rec == null)
			return null;

		GUIForm form = new GUIForm();
		form.setId(Long.parseLong(rec.getAttributeAsString("id")));
		form.setName(rec.getAttributeAsString("name"));
		return form;
	}

	public void refresh() {
		list.refresh(new FormsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FORM;
		detailsContainer.setMembers(details);
	}

	private void onEdit() {
		GUIForm selectedForm = getSelectedForm();
		if (selectedForm != null && details instanceof FormDetailsPanel
				&& ((FormDetailsPanel) details).getForm().getId() == selectedForm.getId())
			((FormDetailsPanel) details).openContentEditor();
	}

	public static String webformURL(String formId) {
		String url = Util.contextPath() + "webform/" + formId;
		return url;
	}
}