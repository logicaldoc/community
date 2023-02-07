package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.data.RightsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
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
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the security policies of a menu.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MenuRightsPanel extends VLayout {

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private ListGrid rightsGrid;

	protected GUIMenu menu;

	boolean withApplyButton = false;

	public MenuRightsPanel(final GUIMenu menu, boolean withApplyButton) {
		this.menu = menu;
		this.withApplyButton = withApplyButton;
	}

	@Override
	public void onDraw() {
		final VLayout container = new VLayout();
		container.setMembersMargin(3);
		addMember(container);

		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID, 50);
		entityId.setCanEdit(false);
		entityId.setHidden(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);

		rightsGrid = new ListGrid();
		rightsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		rightsGrid.setCanFreezeFields(true);
		rightsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		rightsGrid.setAutoFetchData(true);
		rightsGrid.setDataSource(new RightsDS(menu.getId(), false));
		rightsGrid.setFields(entityId, entity);
		rightsGrid.setCanEdit(true);
		rightsGrid.setEditEvent(ListGridEditEvent.CLICK);
		rightsGrid.setModalEditing(true);
		rightsGrid.addCellContextClickHandler((CellContextClickEvent event) -> {
			if (event.getColNum() == 0) {
				Menu contextMenu = setupContextMenu();
				contextMenu.showContextMenu();
			}
			event.cancel();
		});

		container.addMember(rightsGrid);

		HLayout buttons = prepareButtons();
		container.addMember(buttons);
	}

	private HLayout prepareButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		Button applyRights = new Button(I18N.message("applyrights"));
		applyRights.setAutoFit(true);
		applyRights.addClickHandler((ClickEvent event) -> onApply());
		if (withApplyButton)
			buttons.addMember(applyRights);

		addGroupSelector(buttons);

		addUserSelector(buttons);

		return buttons;
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, false, false);
		userForm.setItems(user);

		user.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			// Check if the selected user is already present in the rights
			// table
			ListGridRecord[] records = rightsGrid.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("usergroup"))) {
					user.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("usergroup"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute("read", true);
			rightsGrid.addData(rec);
			user.clearValue();
		});
		buttons.addMember(userForm);
	}

	private void addGroupSelector(HLayout buttons) {
		// Prepare the combo and button for adding a new Group
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
			ListGridRecord[] records = rightsGrid.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("id"))) {
					group.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY, selectedRecord.getAttribute("name"));
			rec.setAttribute(AVATAR, "group");
			rec.setAttribute("read", true);
			rightsGrid.addData(rec);
			group.clearValue();
		});
	}

	/**
	 * Create an array of all rights defined
	 * 
	 * @return array of rights
	 */
	public GUIRight[] getRights() {
		ListGridRecord[] records = rightsGrid.getRecords();
		GUIRight[] tmp = new GUIRight[records.length];

		int i = 0;
		for (ListGridRecord rec : records) {
			GUIRight right = new GUIRight();

			right.setName(rec.getAttributeAsString(ENTITY));
			right.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));

			tmp[i] = right;
			i++;
		}

		return tmp;
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
		deleteItem.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord[] selection = rightsGrid.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
				if (Boolean.TRUE.equals(value)) {
					rightsGrid.removeSelectedData();
					if (!withApplyButton)
						onApply();
				}
			});
		});

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	public void onApply() {
		// Apply all rights
		menu.setRights(getRights());

		SecurityService.Instance.get().applyRights(menu, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				GuiLog.info(I18N.message("appliedrightsmenu"), null);
			}
		});

	}
}