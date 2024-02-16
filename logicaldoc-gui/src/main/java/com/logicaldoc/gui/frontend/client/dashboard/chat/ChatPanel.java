package com.logicaldoc.gui.frontend.client.dashboard.chat;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ChatService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the Chat.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class ChatPanel extends VLayout {

	private DynamicForm postForm;

	public ChatPanel() {
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		ChatMessagesPanel messages = new ChatMessagesPanel();
		messages.setWidth("85%");
		messages.setShowResizeBar(true);

		OnlineUsersPanel onlineUsers = new OnlineUsersPanel();
		onlineUsers.setWidth("15%");

		HLayout body = new HLayout();
		body.setHeight100();
		body.setWidth100();
		body.setMembers(messages, onlineUsers);

		TextItem postMessage = ItemFactory.newTextItem("post", null);
		postMessage.setShowTitle(false);
		postMessage.setBrowserSpellCheck(true);
		postMessage.setWidth("100%");

		ButtonItem postButton = new ButtonItem(I18N.message("post"));
		postButton.setAutoFit(true);
		postButton.setEndRow(true);
		postButton.setStartRow(false);
		postButton.setAlign(Alignment.RIGHT);

		postForm = new DynamicForm();
		postForm.setHeight(30);
		postForm.setWidth100();
		postForm.setNumCols(2);
		postForm.setColWidths("*", "50");
		postForm.setItems(postMessage, postButton);

		setMembers(body, postForm);

		postMessage.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName()))
				onPost(postForm.getValueAsString("post"));
		});

		postButton.addClickHandler(event -> onPost(postForm.getValueAsString("post")));
	}

	protected void onPost(String message) {
		if (message == null || message.trim().isEmpty())
			return;
		ChatService.Instance.get().post(message, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				postForm.clearValue("post");
			}
		});
	}
}