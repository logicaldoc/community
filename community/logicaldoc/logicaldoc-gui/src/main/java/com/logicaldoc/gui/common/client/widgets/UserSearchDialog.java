package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is a form used for quick user selection
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public class UserSearchDialog extends Window {
	private ListGrid grid = new ListGrid();

	private UserSelector selector;

	private ListGridRecord[] lastResult = new ListGridRecord[0];

	public UserSearchDialog(UserSelector selector) {
		this.selector = selector;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("users"));
		setWidth(500);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setMembersMargin(5);
		setAutoSize(true);

		final TextItem username = ItemFactory.newTextItem("username", "username", null);
		final SelectItem group = ItemFactory.newGroupSelector("group", "group");

		ToolStripButton search = new ToolStripButton(I18N.message("search"));
		search.setAutoFit(true);
		search.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				search(username.getValueAsString(), group.getValueAsString());
			}
		});

		final ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		toolStrip.addFormItem(username);
		toolStrip.addFormItem(group);
		toolStrip.addButton(search);

		ListGridField usernameField = new ListGridField("username", I18N.message("username"));
		ListGridField nameField = new ListGridField("firstname", I18N.message("firstname"));
		ListGridField lastnameField = new ListGridField("lastname", I18N.message("lastname"));

		grid.setFields(usernameField, nameField, lastnameField);
		grid.setWidth100();
		grid.setHeight(getHeight());
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setCanFreezeFields(true);
		grid.setAutoFetchData(true);
		grid.setData(lastResult);

		grid.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				ListGridRecord selection = grid.getSelectedRecord();
				onSelect(selection.getAttributeAsLong("id"));
			}
		});

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				grid.setHeight(getHeight() - 68);
			}
		});

		addItem(toolStrip);
		addItem(grid);
	}

	protected void search(String username, String groupId) {
		SecurityService.Instance.get().searchUsers(username, groupId, new AsyncCallback<GUIUser[]>() {
			@Override
			public void onFailure(Throwable caught) {
				ContactingServer.get().hide();
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIUser[] result) {
				lastResult = new ListGridRecord[result.length];
				for (int i = 0; i < result.length; i++) {
					GUIUser hit = result[i];
					ListGridRecord record = new ListGridRecord();
					lastResult[i] = record;
					record.setAttribute("id", hit.getId());
					record.setAttribute("username", hit.getUsername());
					record.setAttribute("firstname", hit.getFirstName());
					record.setAttribute("lastname", hit.getName());
				}

				if (lastResult.length == 1) {
					onSelect(lastResult[0].getAttributeAsLong("id"));
				} else
					grid.setData(lastResult);
			}
		});
	}

	public ListGridRecord[] getLastResult() {
		return lastResult;
	}

	public void onSelect(long id) {
		selector.setValue(Long.toString(id));
		destroy();
	}
}