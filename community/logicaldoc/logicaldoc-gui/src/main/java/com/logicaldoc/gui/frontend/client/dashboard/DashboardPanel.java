package com.logicaldoc.gui.frontend.client.dashboard;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.calendar.UserCalendarPanel;
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

	public static final String WORKFLOW_ID = "workflow";

	public static final String MESSAGES_ID = "messages";

	public static final String TAGS_ID = "tags";

	public static final String USER_ID = "user";

	public static final String CALENDAR_ID = "calendar";

	private static DashboardPanel instance;

	private TabSet tabSet = new TabSet();

	private Tab calendarTab = null;

	private Tab workflowTab = null;

	private Tab messagesTab = null;

	private Tab userTab = null;

	private Tab tagsTab = null;

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

		messagesTab = new Tab(I18N.message("messages"));
		messagesTab.setID(MESSAGES_ID);
		messagesTab.setPane(new MessagesPanel());

		workflowTab = new Tab(I18N.message("workflow"));
		workflowTab.setID(WORKFLOW_ID);
		workflowTab.setPane(new WorkflowDashboard());

		calendarTab = new Tab(I18N.message("calendar"));
		calendarTab.setID(CALENDAR_ID);
		calendarTab.setPane(UserCalendarPanel.get());

		tabSet.addTab(userTab);

		if (Feature.visible(Feature.TAGS)) {
			tabSet.addTab(tagsTab);
			if (!Feature.enabled(Feature.TAGS))
				tagsTab.setPane(new TagsDashboard());
		}

		if (Feature.visible(Feature.MESSAGES)) {
			tabSet.addTab(messagesTab);
			if (!Feature.enabled(Feature.MESSAGES))
				messagesTab.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.CALENDAR)) {
			tabSet.addTab(calendarTab);
			if (!Feature.enabled(Feature.CALENDAR))
				workflowTab.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.WORKFLOW)) {
			tabSet.addTab(workflowTab);
			if (!Feature.enabled(Feature.WORKFLOW))
				workflowTab.setPane(new FeatureDisabled());
		}

		setMembers(tabSet);

		tabSet.selectTab(defaultOpenTab);
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
		tabSet.setTabPane(MESSAGES_ID, new MessagesPanel());
		tabSet.selectTab(MESSAGES_ID);
	}

	public void updateWorkflowTab() {
		tabSet.setTabPane(WORKFLOW_ID, new WorkflowDashboard());
		tabSet.selectTab(WORKFLOW_ID);
	}

	public String getDefaultOpenTab() {
		return defaultOpenTab;
	}

	public void setDefaultOpenTab(String defaultOpenTab) {
		this.defaultOpenTab = defaultOpenTab;
	}
}