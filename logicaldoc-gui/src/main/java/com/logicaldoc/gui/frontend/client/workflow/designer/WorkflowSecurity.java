package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowAclDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This window shows the security policies of the Workflow.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WorkflowSecurity extends Window {

	private static final String WRITE = "write";

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private WorkflowAclDS dataSource;

	private ListGrid list;

	private VLayout container = new VLayout();

	private GUIWorkflow workflow;

	public WorkflowSecurity(GUIWorkflow workflow) {
		this.workflow = workflow;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("workflowsecurity"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(600);
		setHeight(300);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addItem(container);
		refresh(workflow);
	}

	void refresh(GUIWorkflow workflow) {
		this.workflow = workflow;
		container.removeMembers(container.getMembers());

		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField("read", I18N.message("read"), 80);
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);
		read.setAutoFitWidth(true);

		ListGridField write = new ListGridField(WRITE, I18N.message(WRITE), 80);
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);
		write.setAutoFitWidth(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setMinHeight(200);
		list.setMinWidth(300);

		dataSource = new WorkflowAclDS(Long.parseLong(workflow.getId()));
		list.setDataSource(dataSource);

		List<ListGridField> fields = new ArrayList<>();
		fields.add(entityId);
		fields.add(entity);
		fields.add(read);
		fields.add(write);

		list.setFields(fields.toArray(new ListGridField[0]));

		container.addMember(list);

		list.addCellContextClickHandler((CellContextClickEvent event) -> {
			if (event.getColNum() == 0) {
				Menu contextMenu = setupContextMenu();
				contextMenu.showContextMenu();
			}
			event.cancel();
		});

		addButtons();
	}

	private void addButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button save = new Button(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler((ClickEvent event) -> onSave());
		buttons.addMember(save);

		// Prepare the combo and button for adding a new Group
		addGroupSelector(buttons);

		// Prepare the combo and button for adding a new User
		addUserSelector(buttons);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler((ClickEvent event) -> GridUtil.exportCSV(list, true));

		Button printButton = new Button(I18N.message("print"));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler((ClickEvent event) -> GridUtil.print(list));
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);
		user.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			/*
			 * Check if the selected user is already present in the rights table
			 */
			ListGridRecord[] records = list.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("usergroup"))) {
					user.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("usergroup"));
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute("read", true);

			list.addData(rec);
			user.clearValue();
		});
		buttons.addMember(userForm);
	}

	private void addGroupSelector(HLayout buttons) {
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		buttons.addMember(groupForm);
		group.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord selectedRecord = group.getSelectedRecord();
			if (selectedRecord == null)
				return;

			// Check if the selected user is already present in the rights
			// table
			ListGridRecord[] records = list.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("id"))) {
					group.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("id"));
			rec.setAttribute(AVATAR, "group");
			rec.setAttribute(ENTITY, selectedRecord.getAttribute("name"));
			rec.setAttribute("read", true);
			list.addData(rec);
			group.clearValue();
		});
	}

	/**
	 * Creates an array of all the ACL
	 * 
	 * @return the list of ACEs
	 */
	public List<GUIAccessControlEntry> getACL() {
		List<GUIAccessControlEntry> acl = new ArrayList<>();

		if (list.getRecords() != null)
			for (ListGridRecord rec : list.getRecords()) {
				GUIAccessControlEntry right = new GUIAccessControlEntry();
				right.setName(rec.getAttributeAsString(ENTITY));
				right.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));
				right.setWrite(rec.getAttributeAsBoolean(WRITE));
				right.setRead(rec.getAttributeAsBoolean("read"));
				acl.add(right);
			}

		return acl;
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}

	/**
	 * Prepares the context menu
	 * 
	 * @return the context menu
	 */
	private Menu setupContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message("ddelete"));
		deleteItem.addClickHandler(event -> onDelete());

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	private void onDelete() {
		ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm))
				list.removeSelectedData();
		});
	}

	public void onSave() {
		workflow.setAccessControlList(getACL());
		WorkflowService.Instance.get().saveACL(workflow, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				destroy();
			}
		});
	}
}