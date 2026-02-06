package com.logicaldoc.gui.frontend.client.document.reading;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.GroupSelectorCombo;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.common.client.widgets.UserSelectorCombo;
import com.logicaldoc.gui.frontend.client.services.ReadingRequestService;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to allow the user to place a reading request
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class ReadingRequestDialog extends StickyWindow {

	private DynamicForm form = new DynamicForm();

	private List<Long> docIds;

	private UserSelectorCombo usersItem;

	private GroupSelectorCombo groupsItem;

	public ReadingRequestDialog(List<Long> docIds) {
		super(I18N.message("requesttoconfirmreading"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		setMinWidth(400);
		centerInPage();

		this.docIds = docIds;
		form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();

		usersItem = new UserSelectorCombo("users", "users", null, true, true);

		groupsItem = new GroupSelectorCombo("groups", "groups");

		TextAreaItem message = ItemFactory.newTextAreaItem("message", null);
		message.setMinHeight(80);
		message.setWidth("*");

		CheckboxItem alertConfirmation = ItemFactory.newCheckbox("notifyreadingconfirmation");
		alertConfirmation.setValue(true);

		form.setItems(usersItem, groupsItem, message, alertConfirmation);

		addItem(form);
		addItem(prepareButtons());
	}

	protected ToolStrip prepareButtons() {
		ToolStripButton sendButton = new ToolStripButton(I18N.message("send"));
		sendButton.addClickHandler(event -> {
			if (form.validate()) {
				LD.contactingServer();
				ReadingRequestService.Instance.get().askReadingConfirmation(docIds, usersItem.getUserIds(),
						groupsItem.getGroupIds(),
						Boolean.parseBoolean(form.getValueAsString("notifyreadingconfirmation")),
						form.getValueAsString("message"), new DefaultAsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								sendButton.enable();
								super.onFailure(caught);
							}

							@Override
							public void handleSuccess(Void arg0) {
								sendButton.enable();
								GuiLog.info(I18N.message("requestreadingsent"));
								destroy();
							}
						});
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(sendButton);

		return toolStrip;
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