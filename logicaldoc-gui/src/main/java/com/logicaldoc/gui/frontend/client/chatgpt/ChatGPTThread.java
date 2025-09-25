package com.logicaldoc.gui.frontend.client.chatgpt;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.menu.MainMenu;
import com.logicaldoc.gui.frontend.client.services.ChatGPTService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Positioning;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to interact with ChatGPT
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class ChatGPTThread extends Window {

	private static final String CHATGPT = "chatgpt";

	private VLayout contents;

	private VLayout messagesBoard = new VLayout();

	private MessageBox lastMessage;

	private static ChatGPTThread instance = new ChatGPTThread();

	private Timer answerPolling;

	public static ChatGPTThread get() {
		return instance;
	}

	private ChatGPTThread() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MINIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(CHATGPT));
		setCanDragResize(true);
		setIsModal(false);
		setPosition(Positioning.ABSOLUTE);
		setTop(MainMenu.get().getBottom() + 5);
		setLeft(WindowUtils.getWidth() - 505);
		setWidth(500);
		setHeight(500);
		setMembersMargin(2);

		addResizedHandler(resize -> messagesBoard.reflow());
		addRestoreClickHandler(restore -> {
			setWidth(500);
			setHeight(500);
			setTop(MainMenu.get().getBottom() + 5);
			setLeft(WindowUtils.getWidth() - 505);
		});
	}

	public void open(String initialQuestion) {
		if (contents != null) {
			removeItem(contents);
			contents.removeMembers(contents.getMembers());
			messagesBoard.removeMembers(messagesBoard.getMembers());
		}

		messagesBoard = new VLayout();
		messagesBoard.setWidth100();
		messagesBoard.setHeight100();
		messagesBoard.setOverflow(Overflow.SCROLL);
		messagesBoard.setMembersMargin(2);

		contents = new VLayout();
		contents.setWidth100();
		contents.setHeight100();
		contents.setAlign(VerticalAlignment.TOP);
		contents.setOverflow(Overflow.SCROLL);
		contents.addMember(messagesBoard);

		lastMessage = null;

		addQuestionBar();

		addItem(contents);

		startThread(initialQuestion);

		show();
	}

	private void addQuestionBar() {
		TextItem question = ItemFactory.newTextItem("question", null);
		question.setShowTitle(false);
		question.setBrowserSpellCheck(true);
		question.setWidth("100%");
		question.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())) {
				onAsk(question);
			}
		});

		FormItemIcon ask = new FormItemIcon();
		ask.setInline(true);
		ask.setInlineIconAlign(Alignment.RIGHT);
		ask.setText(AwesomeFactory.getIconHtml("paper-plane"));
		question.setIcons(ask);
		ask.addFormItemClickHandler(click -> onAsk(question));

		DynamicForm questionForm = new DynamicForm();
		questionForm.setHeight(30);
		questionForm.setWidth100();
		questionForm.setNumCols(1);
		questionForm.setItems(question);

		contents.addMember(questionForm);
	}

	private void onAsk(TextItem question) {
		ask(question.getValueAsString());
		question.clearValue();
	}

	public void appendMessage(String message, String role) {
		lastMessage = new MessageBox(message, role);
		messagesBoard.addMember(lastMessage);
		scrollToLastMessage();
	}

	private void updateLastMessage(String message) {
		lastMessage.updateMessage(message);
		scrollToLastMessage();
	}

	protected void scrollToLastMessage() {
		final Timer timer = new Timer() {
			public void run() {
				messagesBoard.scrollToBottom();
			}
		};
		timer.schedule(100);
	}

	private void ask(String question) {
		appendMessage(question, "user");
		appendMessage("", CHATGPT);
		answerPolling = null;

		ChatGPTService.Instance.get().ask(question, new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(Void result) {
				collectAnswer();
			}
		});
	}

	private void startThread(String question) {
		appendMessage(question, "user");
		appendMessage("", CHATGPT);
		answerPolling = null;

		ChatGPTService.Instance.get().startThread(question, DocumentController.get().getCurrentSelection(),
				new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(Void arg0) {
						collectAnswer();
					}
				});
	}

	private void collectAnswer() {
		answerPolling = new Timer() {
			public void run() {
				ChatGPTService.Instance.get().getAnswer(new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIValue answer) {
						if (answer.getValue() != null) {
							updateLastMessage(answer.getValue());
							if (!"complete".equals(answer.getCode()))
								answerPolling.schedule(100);
						}
					}
				});
			}
		};
		answerPolling.schedule(200);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ChatGPTThread)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}