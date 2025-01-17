package com.logicaldoc.gui.frontend.client.dashboard.messages;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.calendar.UserCalendarPanel;
import com.logicaldoc.gui.frontend.client.dashboard.UserDashboard;
import com.logicaldoc.gui.frontend.client.dashboard.chat.ChatPanel;
import com.logicaldoc.gui.frontend.client.dashboard.reading.ReadingRequestsPanel;
import com.logicaldoc.gui.frontend.client.dashboard.tag.TagsDashboard;
import com.logicaldoc.gui.frontend.client.workflow.WorkflowDashboard;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This is the dashboard container
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DashboardPanel extends VLayout {

	public static final String READINGREQUESTS = "readingrequests";

	public static final String WORKFLOW = "workflow";

	public static final String MESSAGES = "messages";

	public static final String CHAT_ID = "chat";

	public static final String TAGS_ID = "tags";

	public static final String USER_ID = "user";

	public static final String CALENDAR = "calendar";

	private static DashboardPanel instance;

	private TabSet tabSet = new TabSet();

	private Tab calendarTab = null;

	private Tab workflowTab = null;

	private Tab messagesTab = null;

	private Tab userTab = null;

	private Tab tagsTab = null;

	private Tab chatTab = null;

	private Tab readingsTab = null;

	private String defaultOpenTab = USER_ID;

	private DashboardPanel() {
		setOverflow(Overflow.HIDDEN);
	}

	@Override
	public void onDraw() {
		initGUI();
	}

	private void initGUI() {
		userTab = new Tab(I18N.message("user"));
		userTab.setID(USER_ID);
		userTab.setPane(new UserDashboard());

		tagsTab = new Tab(I18N.message("tags"));
		tagsTab.setID(TAGS_ID);
		tagsTab.setPane(new TagsDashboard());

		messagesTab = new Tab(I18N.message(MESSAGES));
		messagesTab.setID(MESSAGES);
		messagesTab.setPane(new MessagesPanel());

		chatTab = new Tab(I18N.message("chat"));
		chatTab.setID(CHAT_ID);
		chatTab.setPane(new ChatPanel());

		workflowTab = new Tab(I18N.message(WORKFLOW));
		workflowTab.setID(WORKFLOW);
		workflowTab.setPane(new WorkflowDashboard());

		calendarTab = new Tab(I18N.message(CALENDAR));
		calendarTab.setID(CALENDAR);
		calendarTab.setPane(UserCalendarPanel.get());

		readingsTab = new Tab(I18N.message(READINGREQUESTS));
		readingsTab.setID(READINGREQUESTS);
		readingsTab.setPane(ReadingRequestsPanel.get());

		tabSet.addTab(userTab);

		addTagsTab();

		addMessagesTab();

		addChatTab();

		addCalendarTab();

		addWorkflowTab();

		addReadingsTab();

		setMembers(tabSet);

		tabSet.selectTab(defaultOpenTab);
	}

	private void addReadingsTab() {
		if (Feature.visible(Feature.READING_CONFIRMATION) && Menu.enabled(Menu.DASHBOARD_READINGS)) {
			tabSet.addTab(readingsTab);
			if (!Feature.enabled(Feature.READING_CONFIRMATION))
				readingsTab.setPane(new FeatureDisabled());
		}
	}

	private void addWorkflowTab() {
		if (Feature.visible(Feature.WORKFLOW) && Menu.enabled(Menu.DASHBOARD_WORKFLOW)) {
			tabSet.addTab(workflowTab);
			if (!Feature.enabled(Feature.WORKFLOW))
				workflowTab.setPane(new FeatureDisabled());
		}
	}

	private void addCalendarTab() {
		if (Feature.visible(Feature.CALENDAR) && Menu.enabled(Menu.DASHBOARD_CALENDAR)) {
			tabSet.addTab(calendarTab);
			if (!Feature.enabled(Feature.CALENDAR))
				calendarTab.setPane(new FeatureDisabled());
		}
	}

	private void addChatTab() {
		if (Feature.visible(Feature.CHAT) && Menu.enabled(Menu.CHAT)) {
			tabSet.addTab(chatTab);
			if (!Feature.enabled(Feature.CHAT))
				chatTab.setPane(new FeatureDisabled());
		}
	}

	private void addMessagesTab() {
		if (Feature.visible(Feature.MESSAGES) && Menu.enabled(Menu.MESSAGES)) {
			tabSet.addTab(messagesTab);
			if (!Feature.enabled(Feature.MESSAGES))
				messagesTab.setPane(new FeatureDisabled());
		}
	}

	private void addTagsTab() {
		if (Feature.visible(Feature.TAGS)) {
			tabSet.addTab(tagsTab);
			if (!Feature.enabled(Feature.TAGS))
				tagsTab.setPane(new TagsDashboard());
		}
	}

	public static DashboardPanel get() {
		if (instance == null)
			instance = new DashboardPanel();
		return instance;
	}

	public TabSet getTabSet() {
		return tabSet;
	}

	public Tab getWorkflowTab() {
		if (workflowTab == null)
			initGUI();
		return workflowTab;
	}

	public Tab getCalendarTab() {
		if (calendarTab == null)
			initGUI();
		return calendarTab;
	}

	public Tab getMessagesTab() {
		if (messagesTab == null)
			initGUI();
		return messagesTab;
	}

	public Tab getUserTab() {
		if (userTab == null)
			initGUI();
		return userTab;
	}

	public void updateUserTab() {
		tabSet.setTabPane(USER_ID, new UserDashboard());
		tabSet.selectTab(USER_ID);
	}

	public void updateTagsTab() {
		tabSet.setTabPane(TAGS_ID, new TagsDashboard());
		tabSet.selectTab(TAGS_ID);
	}

	public void updateMessageTab() {
		tabSet.setTabPane(MESSAGES, new MessagesPanel());
		tabSet.selectTab(MESSAGES);
	}

	public void updateWorkflowTab() {
		tabSet.setTabPane(WORKFLOW, new WorkflowDashboard());
		tabSet.selectTab(WORKFLOW);
	}

	public void updateReadingsTab() {
		tabSet.setTabPane(READINGREQUESTS, new WorkflowDashboard());
		tabSet.selectTab(READINGREQUESTS);
	}

	public String getDefaultOpenTab() {
		return defaultOpenTab;
	}

	public void setDefaultOpenTab(String defaultOpenTab) {
		this.defaultOpenTab = defaultOpenTab;
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