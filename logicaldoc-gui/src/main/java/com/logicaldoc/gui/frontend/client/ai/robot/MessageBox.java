package com.logicaldoc.gui.frontend.client.ai.robot;

import com.google.gwt.core.client.UnsafeNativeLong;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;

/**
 * A box showing a ChatGPT message in a thread
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
class MessageBox extends Label {

	private static final String ACT_END = "]act]";

	private static final String ACT_START = "[act[";

	private static final String ROBOT = "robot";

	private String avatarIcon;

	private String role;

	public MessageBox(String message, String role, String avatar) {
		setAlign(Alignment.LEFT);
		setCanSelectText(true);

		this.role = role;

		String url = ROBOT.equals(role) ? avatar : Util.avatarUrl(Session.get().getUser().getId());
		avatarIcon = "<img src='" + url
				+ "' style='border: 0px; height: 32px; width: 32px; margin-right: 5px; margin-bottom: 3px' />";

		String color = ROBOT.equals(role) ? "white" : "MintCream";
		setBackgroundColor(color);
		setHeight("*");

		setContents(prepareContent(message));
	}

	private String prepareContent(String message) {
		if (!message.isEmpty()) {
			String messageTextElementId = getID() + "_message";

			String copyButton = "<button onclick=\"copy(document.getElementById('" + messageTextElementId
					+ "').innerText);\" style='width:32px'>" + AwesomeFactory.getIconHtml("paste") + "</button>";

			if (isRobotAnswer())
				message = parseRobotAnswer(message);

			return "<table border='0' width='100%'><tr><td style='width:36px; align:center; vertical-align: top;'>"
					+ avatarIcon + (isRobotAnswer() ? copyButton : "") + "</td><td><span id='" + messageTextElementId
					+ "'>" + message + "</span></td></tr></table>";
		} else {
			return "";
		}
	}

	@Override
	protected void onInit() {
		super.onInit();
		declareOpenDocument(DocumentsPanel.get());
	}

	protected boolean isRobotAnswer() {
		return ROBOT.equals(role);
	}

	/**
	 * Processes the answer looking for action tokens like
	 * [act[<b>command</b>|<b>arg1</b>|<b>arg2</b>]act] and replace them with
	 * proper rendering.
	 * 
	 * @param message The original message from the robot
	 * 
	 * @return The elaborated messages with command tokens expanded
	 */
	private String parseRobotAnswer(String message) {
		RegExp p = RegExp.compile("\\[act\\[[a-zA-Z]*\\|.*\\|.*\\]act\\]");
		MatchResult result = p.exec(message);

		if (result != null) {
			for (int i = 0; i < result.getGroupCount(); i++) {
				String match = result.getGroup(i);
				match = match.replace(ACT_START, "");
				match = match.replace(ACT_END, "");
				String link = processCommand(match.split("\\|"));
				message = message.replace(ACT_START + match + ACT_END, link);
			}
		}

		return message;
	}

	private String processCommand(String[] args) {
		String command = args[0];
		String link = "<a style='cursor: pointer' onclick='act" + command + "(";
		if ("OpenDocument".equals(command) || "OpenFolder".equals(command)) {
			link += args[1] + ");'>" + args[2] + "</a>";
		}
		return link;
	}

	public void updateMessage(String message) {
		setContents(prepareContent(message));
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof MessageBox)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@UnsafeNativeLong
	public static native void declareOpenDocument(DocumentsPanel panel) /*-{
		$wnd.actOpenDocument = function(docId) {
			return panel.@com.logicaldoc.gui.frontend.client.document.DocumentsPanel::openInFolder(J)(docId);
		};
	}-*/;
	
	@UnsafeNativeLong
	public static native void declareOpenFolder(FolderNavigator navigator) /*-{
		$wnd.actOpenFolder = function(folderId) {
			return navigator.@com.logicaldoc.gui.frontend.client.folder.FolderNavigator::openFolder(J)(folderId);
		};
	}-*/;
}