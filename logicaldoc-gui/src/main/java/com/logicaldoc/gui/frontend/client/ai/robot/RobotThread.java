package com.logicaldoc.gui.frontend.client.ai.robot;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.chatgpt.ChatGPTThread;
import com.logicaldoc.gui.frontend.client.menu.MainMenu;
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
 * This popup window is used to interact with a Robot
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class RobotThread extends Window {

	private VLayout contents;

	private VLayout messagesBoard = new VLayout();

	private MessageBox lastMessage;

	private GUIRobot robot;

	/**
	 * A map of thread windows, key is the robot's name
	 */
	private static Map<String, RobotThread> threads = new HashMap<>();

	public static RobotThread get(GUIRobot robot) {
		RobotThread thread = threads.get(robot.getName());
		if (thread == null) {
			thread = new RobotThread(robot);
			threads.put(robot.getName(), thread);
		}
		return thread;
	}

	private RobotThread(GUIRobot robot) {
		this.robot = robot;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MINIMIZE_BUTTON, HeaderControls.CLOSE_BUTTON);
		setTitle(robotName());
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

		if(initialQuestion!=null && !initialQuestion.isEmpty())
			ask(initialQuestion);

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
		lastMessage = new MessageBox(message, role, "robot".equals(role) ? robot.getAvatar() : null);
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

	private String robotName() {
		if (robot.getLabel() == null || robot.getLabel().trim().isEmpty())
			return robot.getName();
		else
			return robot.getLabel();
	}

	private void ask(String question) {
		appendMessage(question, "user");
		appendMessage("", "robot");

		RobotService.Instance.get().ask(robot.getId(), question, new DefaultAsyncCallback<>() {

			@Override
			public void handleSuccess(String answer) {
				updateLastMessage(answer);
			}
		});
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